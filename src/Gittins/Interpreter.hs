{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Gittins.Interpreter where

import Gittins.Config
import Gittins.Pretty
import Gittins.Process
import Gittins.Types

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad.Free (Free(..))
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (putDoc)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.SSem  as Sem


interpretAsync :: Config -> Sem.SSem -> Act a -> IO (Config, a)
interpretAsync config workerSemaphore act = do
  consoleLock <- newMVar ()
  let
    go :: Config -> Act a -> IO (Config, a)
    go cfg act' = case act' of

      Free (Log msg a) -> do
        takeMVar consoleLock
        putDoc (prettyLog msg)
        putMVar consoleLock ()
        go cfg a

      Free (LoadConfig f) ->
        go cfg (f cfg)

      Free (SaveConfig cfg' a) ->
        go cfg' a

      Free (Process wd cmd args f) -> do
        result <- Sem.withSem workerSemaphore $ processResult wd cmd args
        go cfg (f result)

      Free (IsWorkingTree path f) -> do
        isDir <- doesDirectoryExist (path </> ".git")
        go cfg (f isDir)

      -- If concurrent threads write config, the second thread wins
      Free (Concurrently a b f) -> do
        ((_, a'), (cfg', b')) <- Async.concurrently (go cfg a) (go cfg b)
        go cfg' $ f a' b'

      Pure a -> return (cfg, a)

    in go config act

runIO :: RuntimeConfig -> Act a -> IO a
runIO (RuntimeConfig workers) act = do
  config <- loadConfig
  workerSemaphore <- Sem.new workers
  (config', a) <- interpretAsync config workerSemaphore act
  saveConfig config'
  return a
