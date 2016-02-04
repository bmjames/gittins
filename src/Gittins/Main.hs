module Gittins.Main where

import Gittins.Config
import Gittins.FilePath
import Gittins.Interpreter
import Gittins.Process
import Gittins.Types

import Data.Either (partitionEithers)
import Data.List (isPrefixOf, nub)
import Data.Maybe (isJust)

import Options.Applicative
import Options.Applicative.Types (Completer(..))

import System.Exit (ExitCode(..))
import Text.Regex  (Regex, matchRegex, mkRegex)

-- | Type alias for options passed through to Git
type GitOpt = String

type Force = Bool

data Opts = Opts RuntimeConfig Command

data Command = Register [FilePath] [GroupId] Force
             | Unregister [FilePath]
             | List [GroupId]
             | AddToGroup [GroupId] [FilePath]
             | RemoveFromGroup [GroupId] [FilePath]
             | Status [GroupId] [GitOpt]
             | Pull [GroupId] [GitOpt]
             | Diff [GroupId] [GitOpt]
             deriving (Eq, Ord, Show)

parseOpts :: Parser Opts
parseOpts = Opts <$> workerOpts <*> subparser (

  -- Manage repositories and groups
     command "register"   (info registerOpts
                                (progDesc "Register one or more repositories"))
  <> command "unregister" (info unregisterOpts
                                (progDesc "Unregister one ore more repositories"))
  <> command "add-group"  (info addToGroupOpts
                                (progDesc "Add one or more repositories to a group"))
  <> command "rm-group"   (info removeFromGroupOpts
                                 (progDesc "Remove one or more repositories from a group"))
  <> command "list"       (info listOpts (progDesc "List registered repositories"))

  -- Git commands
  <> command "status" (info statusOpts fullDesc)
  <> command "pull"   (info pullOpts fullDesc)
  <> command "diff"   (info diffOpts fullDesc))

  where
    registerOpts = Register <$> paths <*> groupIds <*> force
    unregisterOpts = Unregister <$> paths
    addToGroupOpts = AddToGroup <$> groupIds <*> paths
    removeFromGroupOpts = RemoveFromGroup <$> groupIds <*> paths
    listOpts = List <$> groupIds
    groupIds = many $ strOption (short 'g' <> long "group" <> metavar "GROUP"
                                 <> completer completeGroups)
    paths = some (strArgument (metavar "PATH")) <|> pure ["."]
    statusOpts = Status <$> groupIds <*> gitOpts
    pullOpts = Pull <$> groupIds <*> gitOpts
    diffOpts = Diff <$> groupIds <*> gitOpts
    gitOpts = many (strArgument (metavar "GIT_OPT"))
    force = switch (short 'f' <> long "force")
    workerOpts = RuntimeConfig <$>
                 (option auto ( short 'j'
                             <> metavar "CONCURRENCY"
                             <> help "Max number of Git processes to run concurrently" ) <|> pure 1)

completeGroups :: Completer
completeGroups = Completer $ \prefix -> runIO runtimeConfig $ do
  Config repos <- getConfig
  return $ nub $ filter (prefix `isPrefixOf`) (concatMap repoGroups repos)

  where runtimeConfig = RuntimeConfig 1

-- | Entry point for register command
register :: [GroupId] -> [FilePath] -> Force -> Act ()
register groupIds paths force = mapM_ addRepo paths where

  addRepo :: FilePath -> Act ()
  addRepo p = do
    Config repos <- getConfig
    let name = mkRepoName p
    if any ((== p) . repoPath) repos
      then putLog (AlreadyRegistered p)
      else do
        shouldRegister <- isWorkingTree p
        if force || shouldRegister
          then do putLog (Registering p)
                  putConfig $ Config (Repository name p groupIds : repos)
          else putLog (NotAGitRepository p)

-- | Entry point for unregister command
unregister :: [FilePath] -> Act ()
unregister = mapM_ rmRepo where

 rmRepo :: FilePath -> Act ()
 rmRepo p = do
   Config repos <- getConfig
   if all ((/= p) . repoPath) repos
     then putLog (NotRegistered p)
     else do
       putLog (Unregistering p)
       let repos' = filter ((/= p) . repoPath) repos
       putConfig (Config repos')

-- | Entry point for add-to-group command
addToGroup :: [GroupId] -> [FilePath] -> Act ()
addToGroup groupIds = mapM_ addGroup where

  addGroup :: FilePath -> Act ()
  addGroup path = do
    config <- getConfig
    case addToGroups groupIds path config of
      Just c  -> putConfig c
      Nothing -> putLog (NotRegistered path)

-- | Entry point for remove-from-group command
removeFromGroup :: [GroupId] -> [FilePath] -> Act ()
removeFromGroup groupIds = mapM_ rmGroup where

  rmGroup :: FilePath -> Act ()
  rmGroup path = do
    config <- getConfig
    let config' = modifyRepository (\(Repository n p gs) ->
          if p == path || n == path
             then Just $ Repository n p (filter (not . flip elem groupIds) gs)
             else Nothing) config
    case config' of
      Just c  -> putConfig c
      Nothing -> putLog (NotRegistered path)

-- | Entry point for list command
list :: [GroupId] -> Act ()
list groupIds = do
  Config repos <- getConfig
  let repos' = filter (\(Repository _ _ gs) -> null groupIds || any (`elem` groupIds) gs) repos
  putLog (RepositoriesSummary repos')

-- | Entry point for status command
status :: [GroupId] -> [GitOpt] -> Act ()
status = gitCommand "status"

-- | Entry point for pull command
pull :: [GroupId] -> [GitOpt] -> Act ()
pull groupIds gitOpts = do
  repos <- getReposForGroup groupIds
  outputs <- concurrentFor repos $ \repo@(Repository _ path _) ->
               do result <- git path "pull" gitOpts
                  putLog (GitOutput repo result)
                  return (repo, result)
  putLog (pullSummary outputs)

  where
    pullSummary = uncurry PullSummary
                . partitionEithers
                . map (uncurry toEither)
                . filter (not . isUpToDate . snd)

    isUpToDate (ProcessResult ExitSuccess out _) =
      out == "Already up-to-date.\n" || isJust (matchRegex upToDateRegex out)
    isUpToDate _ = False

    toEither repo (ProcessResult exit _ _) =
      case exit of ExitSuccess   -> Right repo
                   ExitFailure _ -> Left repo

upToDateRegex :: Regex
upToDateRegex = mkRegex "Current branch .+ is up to date."

-- | Entry point for diff
diff :: [GroupId] -> [GitOpt] -> Act ()
diff = gitCommand "diff"

gitCommand :: String -> [GroupId] -> [GitOpt] -> Act ()
gitCommand cmd groupIds gitOpts = do
  repos <- getReposForGroup groupIds
  concurrentFor_ repos $ \repo@(Repository _ p _) ->
    do result <- git p cmd gitOpts
       putLog (GitOutput repo result)

-- | Git command
git :: FilePath -> String -> [GitOpt] -> Act ProcessResult
git cwd cmd opts = process cwd "git" (cmd : opts)

-- | Main entry point
gittinsMain :: IO ()
gittinsMain = do
  Opts runtimeConfig cmd <- execParser $ info (helper <*> parseOpts) fullDesc
  let run = runIO runtimeConfig
  case cmd of
    Register paths groupIds force  -> do paths' <- canonicalize paths
                                         run $ register groupIds paths' force
    Unregister paths               -> canonicalize paths >>= run . unregister
    AddToGroup groupIds paths      -> canonicalize paths >>= run . addToGroup groupIds
    RemoveFromGroup groupIds paths -> canonicalize paths >>= run . removeFromGroup groupIds
    List groupIds                  -> run $ list groupIds
    Status groupIds gitOpts        -> run $ status groupIds gitOpts
    Pull groupIds gitOpts          -> run $ pull groupIds gitOpts
    Diff groupIds gitOpts          -> run $ diff groupIds gitOpts

  where
    canonicalize = mapM safeCanonicalize
