{-# LANGUAGE DeriveFunctor #-}

module Gittins.Types where

import Gittins.Config
import Gittins.Pretty

import Control.Monad (unless)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.State.Lazy (StateT, get, liftIO, put, runStateT)
import System.IO (hGetContents)
import System.Process (CreateProcess(..), CmdSpec(RawCommand), StdStream(..), createProcess)


data Act' a = Log LogMessage a
            | LoadConfig (Config -> a)
            | SaveConfig Config a
            | Shell CreateProcess (String -> a)
            deriving Functor

type Act a = Free Act' a

putLog :: LogMessage -> Act ()
putLog msg = liftF (Log msg ())

getConfig :: Act Config
getConfig = liftF (LoadConfig id)

putConfig :: Config -> Act ()
putConfig config = liftF (SaveConfig config ())

shell :: FilePath -> FilePath -> [String] -> Act String
shell cwd cmd args = liftF (Shell cp id) where
  cp = CreateProcess { cmdspec = RawCommand cmd args
                     , cwd = Just cwd
                     , env = Nothing
                     , std_in = Inherit
                     , std_out = CreatePipe
                     , std_err = Inherit
                     , close_fds = False
                     , create_group = False
                     , delegate_ctlc = False
                     }

getReposForGroup :: [GroupId] -> Act [Repository]
getReposForGroup groupIds = do
  Config repos <- getConfig
  return $ filter (\(Repository _ gs) -> null groupIds || any (`elem` groupIds) gs) repos

interpretStateTIO :: Act a -> StateT Config IO a
interpretStateTIO act = case act of
  Free (Log msg a) -> do
    liftIO (putDoc (prettyLog msg) >> putStrLn "")
    interpretStateTIO a

  Free (LoadConfig f) -> do
    config <- get
    interpretStateTIO (f config)

  Free (SaveConfig c a) -> do
    put c
    interpretStateTIO a

  Free (Shell cp f) -> do
    out <- liftIO $ do
      (_, Just hOut, _, _) <- liftIO (createProcess cp)
      hGetContents hOut
    interpretStateTIO (f out)

  Pure a -> return a

runIO :: Act a -> IO a
runIO act = do
  config       <- loadConfig
  (a, config') <- runStateT (interpretStateTIO act) config
  unless (config == config') (saveConfig config')
  return a

data LogMessage = AlreadyRegistered FilePath
                | NotRegistered FilePath
                | Registering FilePath
                | Unregistering FilePath
                | RepositoriesSummary [Repository]
                | StatusSummary [(Repository, String)]
                | PullSummary [(Repository, String)]
                deriving (Eq, Ord, Show)

prettyLog :: LogMessage -> Doc
prettyLog msg = case msg of
  AlreadyRegistered path -> logMessage $ "Path [" ++ path ++ "] is already registered."
  NotRegistered path     -> logMessage $ "Path [" ++ path ++ "] does not appear to be registered."
  Registering path       -> logMessage $ "Registering [" ++ path ++ "]"
  Unregistering path     -> logMessage $ "Unregistering [" ++ path ++ "]"
  StatusSummary rs       -> vcat $ map (\(r, out) -> summary (repoName r) out) rs
  PullSummary rs         -> vcat $ map (\(r, out) -> summary (repoName r) out) rs
  RepositoriesSummary rs -> list $ map repoName rs
