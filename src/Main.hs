module Main where

import Config
import Data.Foldable (foldlM)
import Data.List (delete)
import Data.Text (pack)
import Options.Applicative
import System.Directory (canonicalizePath)

data Opts = Register [FilePath]
          | Unregister [FilePath]
          | AddToGroup GroupId [FilePath]
          | RemoveFromGroup GroupId [FilePath]
          deriving (Eq, Ord, Show)

parseOpts :: Parser Opts
parseOpts = subparser $
     command "register" (info registerOpts
                              (progDesc "Register one or more repositories"))
  <> command "unregister" (info unregisterOpts
                                (progDesc "Unregister one ore more repositories"))
  <> command "add-to-group" (info addToGroupOpts
                                  (progDesc "Add one ore more repositories to a group"))
  <> command "remove-from-group" (info removeFromGroupOpts
                                       (progDesc "Remove one or more repositories from a group"))

  where
    registerOpts = Register <$> paths
    unregisterOpts = Unregister <$> paths
    addToGroupOpts = AddToGroup <$> groupId <*> paths
    removeFromGroupOpts = RemoveFromGroup <$> groupId <*> paths
    groupId = pack <$> strArgument (metavar "GROUP")
    paths = some (strArgument (metavar "PATH"))

-- | Entry point for register command
register :: [FilePath] -> IO ()
register paths = modifyConfig $ \config -> foldlM addRepo config paths

  where
    addRepo :: Config -> FilePath -> IO Config
    addRepo config@(Config repos) p = do
      canonical <- canonicalizePath p
      if any (\r -> path r == canonical) repos
        then do putStrLn ("Path [" ++ canonical ++ "] is already registered.")
                return config
        else do putStrLn ("Registering [" ++ canonical ++ "]")
                let repo = Repository canonical []
                return $ Config (repo : repos)

-- | Entry point for unregister command
unregister :: [FilePath] -> IO ()
unregister paths = modifyConfig $ \config -> foldlM rmRepo config paths

  where
    rmRepo :: Config -> FilePath -> IO Config
    rmRepo config@(Config repos) p = do
      canonical <- canonicalizePath p
      if not $ any (\r -> path r == canonical) repos
         then do putStrLn ("Path [" ++ canonical ++ "] does not appear to be registered.")
                 return config
         else do putStrLn ("Unregistering [" ++ canonical ++ "]")
                 let repos' = filter (\r -> path r /= canonical) repos
                 return $ Config repos'

-- | Entry point for add-to-group command
addToGroup :: GroupId -> [FilePath] -> IO ()
addToGroup groupId paths = modifyConfig $ \config -> foldlM addGroup config paths

  where
    addGroup :: Config -> FilePath -> IO Config
    addGroup config path = do
      canonical <- canonicalizePath path
      let config' = modifyRepository (\(Repository p gs) ->
            if p == canonical then Just $ Repository p (groupId:gs) else Nothing) config
      case config' of
        Just c  -> return c
        Nothing -> do putStrLn ("Path [" ++ canonical ++ "] does not appear to be registered.")
                      return config

-- | Entry point for remove-from-group command
removeFromGroup :: GroupId -> [FilePath] -> IO ()
removeFromGroup groupId paths = modifyConfig $ \config -> foldlM removeGroup config paths

  where
    removeGroup :: Config -> FilePath -> IO Config
    removeGroup config path = do
      canonical <- canonicalizePath path
      let config' = modifyRepository (\(Repository p gs) ->
            if p == canonical then Just $ Repository p (delete groupId gs) else Nothing) config
      case config' of
        Just c  -> return c
        Nothing -> do putStrLn ("Path [" ++ canonical ++ "] does not appear to be registered.")
                      return config

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOpts) fullDesc
  case opts of
    Register paths                -> register paths
    Unregister paths              -> unregister paths
    AddToGroup groupId paths      -> addToGroup groupId paths
    RemoveFromGroup groupId paths -> removeFromGroup groupId paths
