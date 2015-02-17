module Main where

import Config
import Data.Foldable (foldlM)
import Data.Text (pack)
import Options.Applicative
import System.Directory (canonicalizePath)

data Opts = Register [FilePath] [GroupId]
          | Unregister [FilePath]
          | AddToGroup [GroupId] [FilePath]
          | RemoveFromGroup [GroupId] [FilePath]
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
    registerOpts = Register <$> paths <*> groupIds
    unregisterOpts = Unregister <$> paths
    addToGroupOpts = AddToGroup <$> liftA2 (:) groupId groupIds <*> paths
    removeFromGroupOpts = RemoveFromGroup <$> liftA2 (:) groupId groupIds <*> paths
    groupIds = many $ pack <$> strOption (short 'g' <> long "group" <> metavar "GROUP")
    groupId = pack <$> strArgument (metavar "GROUP")
    paths = some $ strArgument (metavar "PATH")

-- | Entry point for register command
register :: [FilePath] -> [GroupId] -> IO ()
register paths groupIds = modifyConfig $ \config -> foldlM addRepo config paths

  where
    addRepo :: Config -> FilePath -> IO Config
    addRepo config@(Config repos) p = do
      canonical <- canonicalizePath p
      if any (\r -> path r == canonical) repos
        then do putStrLn ("Path [" ++ canonical ++ "] is already registered.")
                return config
        else do putStrLn ("Registering [" ++ canonical ++ "]")
                let repo = Repository canonical groupIds
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
addToGroup :: [GroupId] -> [FilePath] -> IO ()
addToGroup groupIds paths = modifyConfig $ \config -> foldlM addGroup config paths

  where
    addGroup :: Config -> FilePath -> IO Config
    addGroup config path = do
      canonical <- canonicalizePath path
      let config' = addToGroups groupIds canonical config
      case config' of
        Just c  -> return c
        Nothing -> do putStrLn ("Path [" ++ canonical ++ "] does not appear to be registered.")
                      return config

-- | Entry point for remove-from-group command
removeFromGroup :: [GroupId] -> [FilePath] -> IO ()
removeFromGroup groupIds paths = modifyConfig $ \config -> foldlM removeGroup config paths

  where
    removeGroup :: Config -> FilePath -> IO Config
    removeGroup config path = do
      canonical <- canonicalizePath path
      let config' = modifyRepository (\(Repository p gs) ->
            if p == canonical
               then Just $ Repository p (filter (not . flip elem groupIds) gs)
               else Nothing) config
      case config' of
        Just c  -> return c
        Nothing -> do putStrLn ("Path [" ++ canonical ++ "] does not appear to be registered.")
                      return config

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOpts) fullDesc
  case opts of
    Register paths groupIds        -> register paths groupIds
    Unregister paths               -> unregister paths
    AddToGroup groupIds paths      -> addToGroup groupIds paths
    RemoveFromGroup groupIds paths -> removeFromGroup groupIds paths
