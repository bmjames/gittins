{-# LANGUAGE OverloadedStrings #-}

module Gittins.Config (
    Config(..)
  , GroupId
  , Repository(..)
  , repoName
  , loadConfig
  , saveConfig
  , modifyConfig
  , modifyRepository
  , addToGroups
) where

import Control.Applicative ((<|>))
import Control.Monad.Par (NFData)
import Data.Foldable (foldMap)
import Data.HashMap.Strict (HashMap, empty, foldrWithKey, fromList)
import Data.Ini (Ini(..), readIniFile, writeIniFile)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>), takeFileName)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


loadConfig :: IO Config
loadConfig = fmap fromIni loadIni

saveConfig :: Config -> IO ()
saveConfig config = do iniPath <- configFile
                       writeIniFile iniPath (toIni config)

modifyConfig :: (Config -> IO Config) -> IO ()
modifyConfig f = loadConfig >>= f >>= saveConfig

data Config = Config { repositories :: [Repository] }
            deriving (Eq, Ord, Show)

instance NFData Config

type GroupId = String

data Repository = Repository { repoPath :: FilePath, repoGroups :: [GroupId] }
                deriving (Eq, Ord, Show)

-- | Short name for a repository (just the last part of the path)
repoName :: Repository -> String
repoName = takeFileName . repoPath

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
                                       (foldMap parseGroups $ HM.lookup "group" props)

    parseGroups :: Text -> [GroupId]
    parseGroups = map unpack . nub . T.words

toIni :: Config -> Ini
toIni (Config repos) = Ini $ fromList (map fromRepository repos)

  where
    fromRepository :: Repository -> (Text, HashMap Text Text)
    fromRepository repo@(Repository p _) = (pack p, settings repo)

    settings :: Repository -> HashMap Text Text
    settings (Repository _ gs) =
      let group = if null gs then Nothing else Just ("group", T.pack $ unwords (nub gs))
      in fromList (catMaybes [group])

-- | Given a function from 'Repository' to 'Maybe' 'Repository', returns the
-- first 'Just' result obtained by applying the function to each 'Repository',
-- or 'Nothing' otherwise.
modifyRepository :: (Repository -> Maybe Repository) -> Config -> Maybe Config
modifyRepository f (Config repos) = fmap Config (go repos) where
  go (r:rs) = fmap (:rs) (f r) <|> fmap (r:) (go rs)
  go [] = Nothing

addToGroups :: [GroupId] -> FilePath -> Config -> Maybe Config
addToGroups groupIds path = modifyRepository $ \(Repository p gs) ->
  if p == path then Just $ Repository p (nub $ groupIds ++ gs) else Nothing
