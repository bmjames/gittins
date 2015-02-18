module Main where

import Config
import Types

import Data.Foldable (forM_)
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
register :: [GroupId] -> [FilePath] -> Act ()
register groupIds paths = getConfig >>= forM_ paths . addRepo

  where
    addRepo :: Config -> FilePath -> Act ()
    addRepo (Config repos) p
      | any ((== p) . path) repos =
          logInfo $ "Path [" ++ p ++ "] is already registered."
      | otherwise = do
          logInfo $ "Registering [" ++ p ++ "]"
          putConfig $ Config (Repository p groupIds : repos)

-- | Entry point for unregister command
unregister :: [FilePath] -> Act ()
unregister paths = getConfig >>= forM_ paths . rmRepo

  where
    rmRepo :: Config -> FilePath -> Act ()
    rmRepo (Config repos) p
      | all ((/= p) . path) repos =
          logInfo $ "Path [" ++ p ++ "] does not appear to be registered."
      | otherwise = do
          logInfo $ "Unregistering [" ++ p ++ "]"
          let repos' = filter (\r -> path r /= p) repos
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
        Nothing -> logInfo $ "Path [" ++ path ++ "] does not appear to be registered."

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
        Nothing -> logInfo $ "Path [" ++ path ++ "] does not appear to be registered."

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOpts) fullDesc
  case opts of
    Register paths groupIds        -> mapM canonicalizePath paths >>= runIO . register groupIds
    Unregister paths               -> mapM canonicalizePath paths >>= runIO . unregister
    AddToGroup groupIds paths      -> mapM canonicalizePath paths >>= runIO . addToGroup groupIds
    RemoveFromGroup groupIds paths -> mapM canonicalizePath paths >>= runIO . removeFromGroup groupIds
