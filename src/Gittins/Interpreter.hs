{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Gittins.Interpreter where

import Gittins.Config
import Gittins.Pretty
import Gittins.Process
import Gittins.Types

import Control.Monad.Free (Free(..))
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (putDoc)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.SSem  as Sem


interpretAsync :: Config -> RuntimeConfig -> Act a -> IO (Config, a)
interpretAsync config (RuntimeConfig workers) act = do
  consoleLock <- Sem.new 1
  processSem  <- Sem.new workers
  let
    go :: Config -> Act a -> IO (Config, a)
    go cfg act' = case act' of

      Free (Log msg a) -> do
        Sem.withSem consoleLock $ putDoc $ prettyLog msg
        go cfg a

      Free (LoadConfig f) ->
        go cfg (f cfg)

      Free (SaveConfig cfg' a) ->
        go cfg' a

      Free (Process wd cmd args f) -> do
        result <- Sem.withSem processSem $ processResult wd cmd args
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
runIO runtimeConfig act = do
  config <- loadConfig
  (config', a) <- interpretAsync config runtimeConfig act
  saveConfig config'
  return a
