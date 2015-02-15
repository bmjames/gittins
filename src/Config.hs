{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config(..)
  , Repository(..)
  , loadConfig
  , saveConfig
  , modifyConfig
) where

import Data.HashMap.Strict (HashMap, empty, foldrWithKey, fromList)
import Data.Ini (Ini(..), readIniFile, writeIniFile)
import Data.Text (Text, pack, unpack)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

loadConfig :: IO Config
loadConfig = fmap fromIni loadIni

saveConfig :: Config -> IO ()
saveConfig config = do iniPath <- configFile
                       writeIniFile iniPath (toIni config)

modifyConfig :: (Config -> IO Config) -> IO ()
modifyConfig f = loadConfig >>= f >>= saveConfig

data Config = Config { repositories :: [Repository] } deriving (Eq, Ord, Show)

data Repository = Repository { path :: FilePath } deriving (Eq, Ord, Show)

configFile :: IO FilePath
configFile = do homeDir <- getHomeDirectory
                return (homeDir </> ".gittins")

loadIni :: IO Ini
loadIni = do iniPath <- configFile
             fileExists <- doesFileExist iniPath
             ini <- if fileExists
                      then readIniFile iniPath
                      else return $ Right (Ini empty)
             case ini of
               Right i -> return i
               Left _  -> error $ "Failed to parse INI format [" ++ iniPath ++ "]"

fromIni :: Ini -> Config
fromIni (Ini repos) =
  Config $ foldrWithKey (\n ps -> (repository n ps :)) [] repos

  where
    repository :: Text -> HashMap Text Text -> Repository
    repository path props = Repository (unpack path)

toIni :: Config -> Ini
toIni (Config repos) = Ini $ fromList (map fromRepository repos)

  where
    fromRepository (Repository path) = (pack path, empty)
