{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeFamilies #-}

module Gittins.Types where

import Gittins.Config
import Gittins.Pretty

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par (NFData)
import Control.Monad.Par.IO (ParIO, runParIO)
import Control.Monad.Trans.Control (MonadBaseControl(..), liftBaseDiscard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import System.Exit (ExitCode)
import System.IO (hGetContents)
import System.Process (CreateProcess(..), CmdSpec(RawCommand), StdStream(..),
                       createProcess, waitForProcess)

import qualified Control.Monad.Par.Class as Par

data Act' a where
  Log :: LogMessage -> a -> Act' a
  LoadConfig :: (Config -> a) -> Act' a
  SaveConfig :: Config -> a -> Act' a
  Process :: FilePath -> FilePath -> [String] -> (ProcessResult -> a) -> Act' a
  Concurrently :: (NFData b, NFData c) => Act b -> Act c -> (b -> c -> a) -> Act' a

instance Functor Act' where
  fmap f a = case a of
    Log msg a1 -> Log msg (f a1)
    LoadConfig g -> LoadConfig (f . g)
    SaveConfig c a1 -> SaveConfig c (f a1)
    Process wd cmd args g -> Process wd cmd args (f . g)
    Concurrently b1 b2 g -> Concurrently b1 b2 (fmap f . g)

type Act a = Free Act' a

putLog :: LogMessage -> Act ()
putLog msg = liftF (Log msg ())

getConfig :: Act Config
getConfig = liftF (LoadConfig id)

putConfig :: Config -> Act ()
putConfig config = liftF (SaveConfig config ())

process :: FilePath -> FilePath -> [String] -> Act ProcessResult
process wd cmd args = liftF (Process wd cmd args id)

concurrently :: (NFData a, NFData b) => Act a -> Act b -> Act (a, b)
concurrently a1 a2 = liftF (Concurrently a1 a2 (,))

concurrentSeq :: NFData a => [Act a] -> Act [a]
concurrentSeq = foldr (fmap (fmap (uncurry (:))) . concurrently) (return [])

concurrentSeq_ :: [Act ()] -> Act ()
concurrentSeq_ = foldr (fmap void . concurrently) (return ())

concurrentFor_ :: [a] -> (a -> Act ()) -> Act ()
concurrentFor_ as f = concurrentSeq_ $ map f as

getReposForGroup :: [GroupId] -> Act [Repository]
getReposForGroup groupIds = do
  Config repos <- getConfig
  return $ filter (\(Repository _ gs) -> null groupIds || any (`elem` groupIds) gs) repos

instance MonadBase ParIO ParIO

instance MonadBaseControl ParIO ParIO where
  type StM ParIO a = a
  liftBaseWith f = f id
  restoreM = return

interpretParIO :: Act a -> StateT Config ParIO a
interpretParIO act = do
  consoleLock <- liftIO $ newMVar ()
  let
    go :: Act a -> StateT Config ParIO a
    go act' = case act' of

      Free (Log msg a) -> do
        liftIO $ do
          takeMVar consoleLock
          putDoc (prettyLog msg)
          putStrLn ""
          putMVar consoleLock ()
        go a

      Free (LoadConfig f) -> go . f =<< get

      Free (SaveConfig c a) -> do
        put c
        go a

      Free (Process wd cmd args f) -> do
        result <- liftIO $ do
          (_, Just hOut, Just hErr, ph) <- createProcess (mkProcess wd cmd args)
          out <- hGetContents hOut
          err <- hGetContents hErr
          exitCode <- waitForProcess ph
          return $ ProcessResult exitCode out err
        go (f result)

      Free (Concurrently a1 a2 f) -> do
        ivar <- lift Par.new
        liftBaseDiscard Par.fork $ go a1 >>= lift . Par.put ivar
        a2' <- go a2
        -- If concurrent threads write config, the first thread wins
        a1' <- lift $ Par.get ivar
        go (f a1' a2')

      Pure a -> return a
    in go act

mkProcess :: FilePath -> FilePath -> [String] -> CreateProcess
mkProcess workingDir cmd args =
  CreateProcess { cmdspec = RawCommand cmd args
                , cwd = Just workingDir
                , env = Nothing
                , std_in = Inherit
                , std_out = CreatePipe
                , std_err = CreatePipe
                , close_fds = False
                , create_group = False
                , delegate_ctlc = False
                }

runIO :: Act a -> IO a
runIO act = do
  config <- loadConfig
  (a, config') <- runParIO $ runStateT (interpretParIO act) config
  saveConfig config'
  return a

data ProcessResult = ProcessResult ExitCode String String
                   deriving (Eq, Ord, Show)

data LogMessage = AlreadyRegistered FilePath
                | NotRegistered FilePath
                | Registering FilePath
                | Unregistering FilePath
                | RepositoriesSummary [Repository]
                | PullSummary [(Repository, ProcessResult)]
                | StatusSummary [(Repository, ProcessResult)]
                | ProcessError String
                deriving (Eq, Ord, Show)

prettyLog :: LogMessage -> Doc
prettyLog msg = case msg of
  AlreadyRegistered path -> logMessage $ "Path [" ++ path ++ "] is already registered."
  NotRegistered path     -> logMessage $ "Path [" ++ path ++ "] does not appear to be registered."
  Registering path       -> logMessage $ "Registering [" ++ path ++ "]"
  Unregistering path     -> logMessage $ "Unregistering [" ++ path ++ "]"
  PullSummary rs         -> vcat $ map (\(r, ProcessResult _ out err) -> summary (repoName r) out err) rs
  StatusSummary rs       -> vcat $ map (\(r, ProcessResult _ out err) -> summary (repoName r) out err) rs
  RepositoriesSummary rs -> list $ map repoName rs
  ProcessError e         -> logMessage e
