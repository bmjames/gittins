module Gittins.Main where

import Gittins.Config
import Gittins.Types

import Data.List (isPrefixOf, nub)
import Data.Foldable (forM_)
import Options.Applicative
import Options.Applicative.Types (Completer(..))
import System.Directory (canonicalizePath)


-- | Type alias for options passed through to Git
type GitOpt = String

data Opts = Register [FilePath] [GroupId]
          | Unregister [FilePath]
          | List [GroupId]
          | AddToGroup [GroupId] [FilePath]
          | RemoveFromGroup [GroupId] [FilePath]
          | Status [GroupId] [GitOpt]
          | Pull [GroupId] [GitOpt]
          deriving (Eq, Ord, Show)

parseOpts :: Parser Opts
parseOpts = subparser $

  -- Manage repositories and groups
     command "register" (info registerOpts
                              (progDesc "Register one or more repositories"))
  <> command "unregister" (info unregisterOpts
                                (progDesc "Unregister one ore more repositories"))
  <> command "add-to-group" (info addToGroupOpts
                                  (progDesc "Add one or more repositories to a group"))
  <> command "remove-from-group" (info removeFromGroupOpts
                                       (progDesc "Remove one or more repositories from a group"))
  <> command "list" (info listOpts (progDesc "List registered repositories"))

  -- Git commands
  <> command "status" (info statusOpts fullDesc)
  <> command "pull"   (info pullOpts fullDesc)

  where
    registerOpts = Register <$> paths <*> groupIds
    unregisterOpts = Unregister <$> paths
    addToGroupOpts = AddToGroup <$> groupIds <*> paths
    removeFromGroupOpts = RemoveFromGroup <$> groupIds <*> paths
    listOpts = List <$> groupIds
    groupIds = many $ strOption (short 'g' <> long "group" <> metavar "GROUP"
                                 <> completer completeGroups)
    paths = some (strArgument (metavar "PATH")) <|> pure ["."]
    statusOpts = Status <$> groupIds <*> gitOpts
    pullOpts = Pull <$> groupIds <*> gitOpts
    gitOpts = many (strArgument (metavar "GIT_OPT"))

completeGroups :: Completer
completeGroups = Completer $ \prefix -> runIO $ do
  Config repos <- getConfig
  return $ nub $ filter (prefix `isPrefixOf`) (concatMap repoGroups repos)

-- | Entry point for register command
register :: [GroupId] -> [FilePath] -> Act ()
register groupIds = mapM_ addRepo where

  addRepo :: FilePath -> Act ()
  addRepo p = do
    Config repos <- getConfig
    if any ((== p) . repoPath) repos
      then putLog (AlreadyRegistered p)
      else do
        putLog (Registering p)
        putConfig $ Config (Repository p groupIds : repos)

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
    let config' = modifyRepository (\(Repository p gs) ->
          if p == path
             then Just $ Repository p (filter (not . flip elem groupIds) gs)
             else Nothing) config
    case config' of
      Just c  -> putConfig c
      Nothing -> putLog (NotRegistered path)

-- | Entry point for list command
list :: [GroupId] -> Act ()
list groupIds = do
  Config repos <- getConfig
  let repos' = filter (\(Repository _ gs) -> null groupIds || any (`elem` groupIds) gs) repos
  putLog (RepositoriesSummary repos') -- P.list (map repoName repos')

-- | Entry point for status command
status :: [GroupId] -> [GitOpt] -> Act ()
status groupIds gitOpts = do
  repos <- getReposForGroup groupIds
  forM_ repos $ \repo@(Repository p _) ->
    do out <- git p "status" gitOpts
       putLog $ StatusSummary [(repo, out)] -- P.summary (repoName repo)

-- | Entry point for pull command
pull :: [GroupId] -> [GitOpt] -> Act ()
pull groupIds gitOpts = do
  repos <- getReposForGroup groupIds
  forM_ repos $ \repo@(Repository p _) ->
    do out <- git p "pull" gitOpts
       putLog $ PullSummary [(repo, out)] -- P.summary (repoName repo)

-- | Git command
git :: FilePath -> String -> [GitOpt] -> Act String
git cwd cmd opts = shell cwd "git" (cmd : opts)

-- | Main entry point
gittinsMain :: IO ()
gittinsMain = do
  opts <- execParser $ info (helper <*> parseOpts) fullDesc
  case opts of
    Register paths groupIds        -> canonicalizeAll paths >>= runIO . register groupIds
    Unregister paths               -> canonicalizeAll paths >>= runIO . unregister
    AddToGroup groupIds paths      -> canonicalizeAll paths >>= runIO . addToGroup groupIds
    RemoveFromGroup groupIds paths -> canonicalizeAll paths >>= runIO . removeFromGroup groupIds
    List groupIds                  -> runIO $ list groupIds
    Status groupIds gitOpts        -> runIO $ status groupIds gitOpts
    Pull groupIds gitOpts          -> runIO $ pull groupIds gitOpts

  where
    canonicalizeAll = mapM canonicalizePath
