{-# LANGUAGE OverloadedStrings #-}

module Config (
    Config(..)
  , Repository(..)
  , loadConfig
    , loadIni
) where

import Data.HashMap.Strict (HashMap, empty, foldrWithKey, (!))
import Data.Ini (Ini(..), readIniFile)
import Data.Text (Text, unpack)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

loadConfig :: IO Config
loadConfig = fmap parseConfig loadIni

data Config = Config { repositories :: [Repository] } deriving (Eq, Ord, Show)
data Repository = Repository { name :: String, path :: FilePath } deriving (Eq, Ord, Show)

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

parseConfig :: Ini -> Config
parseConfig (Ini repos) =
  Config $ foldrWithKey (\n ps -> (repository n ps :)) [] repos

  where
    repository :: Text -> HashMap Text Text -> Repository
    repository name props = Repository (unpack name) (unpack $ props ! "path")
