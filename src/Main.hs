module Main where

import Config
import Types

import Data.List (isPrefixOf, nub)
import Data.Foldable (forM_)
import Options.Applicative
import Options.Applicative.Types (Completer(..))
import System.Directory (canonicalizePath)
import System.FilePath (takeFileName)

import qualified Pretty as P

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
                                  (progDesc "Add one ore more repositories to a group"))
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
      then logInfo (AlreadyRegistered p)
      else do
        logInfo (Registering p)
        putConfig $ Config (Repository p groupIds : repos)

-- | Entry point for unregister command
unregister :: [FilePath] -> Act ()
unregister = mapM_ rmRepo where

 rmRepo :: FilePath -> Act ()
 rmRepo p = do
   Config repos <- getConfig
   if all ((/= p) . repoPath) repos
     then logInfo (NotRegistered p)
     else do
       logInfo (Unregistering p)
       let repos' = filter ((/= p) . repoPath) repos
       putConfig (Config repos')

-- | Entry point for add-to-group command
addToGroup :: [GroupId] -> [FilePath] -> Act ()
addToGroup groupIds paths = getConfig >>= forM_ paths . addGroup

  where
    addGroup :: Config -> FilePath -> Act ()
    addGroup config path = do
      let config' = addToGroups groupIds path config
      case config' of
        Just c  -> putConfig c
        Nothing -> logInfo (NotRegistered path)

-- | Entry point for remove-from-group command
removeFromGroup :: [GroupId] -> [FilePath] -> Act ()
removeFromGroup groupIds paths = getConfig >>= forM_ paths . rmGroup

  where
    rmGroup :: Config -> FilePath -> Act ()
    rmGroup config path = do
      let config' = modifyRepository (\(Repository p gs) ->
            if p == path
               then Just $ Repository p (filter (not . flip elem groupIds) gs)
               else Nothing) config
      case config' of
        Just c  -> putConfig c
        Nothing -> logInfo (NotRegistered path)

-- | Entry point for list command
list :: [GroupId] -> Act ()
list groupIds = do
  Config repos <- getConfig
  let repos' = filter (\(Repository _ gs) -> null groupIds || any (`elem` groupIds) gs) repos
  printDoc $ P.list (map repoName repos')

-- | Entry point for status command
status :: [GroupId] -> [GitOpt] -> Act ()
status groupIds gitOpts = do
  repos <- getReposForGroup groupIds
  forM_ repos $ \repo@(Repository p _) ->
    git p "status" gitOpts >>= printDoc . P.summary (repoName repo)

-- | Entry point for pull command
pull :: [GroupId] -> [GitOpt] -> Act ()
pull groupIds gitOpts = do
  repos <- getReposForGroup groupIds
  forM_ repos $ \repo@(Repository p _) ->
    git p "pull" gitOpts >>= printDoc . P.summary (repoName repo)

-- | Git command
git :: FilePath -> String -> [GitOpt] -> Act String
git cwd cmd opts = shell cwd "git" (cmd : opts)

-- | Short name for a repository (just the last part of the path)
repoName :: Repository -> String
repoName = takeFileName . repoPath

main :: IO ()
main = do
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
