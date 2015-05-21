{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Gittins.Interpreter where

import Gittins.Config
import Gittins.Pretty
import Gittins.Process
import Gittins.Types

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.IO (ParIO, runParIO)
import Control.Monad.Trans.Control (MonadBaseControl(..), liftBaseDiscard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (putDoc)

import qualified Control.Monad.Par.Class as Par


instance MonadBase ParIO ParIO where
  liftBase = id

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
        result <- liftIO $ processResult wd cmd args
        go (f result)

      Free (IsWorkingTree path f) -> do
        isDir <- liftIO $ doesDirectoryExist (path </> ".git")
        go (f isDir)

      -- If concurrent threads write config, the second thread wins
      Free (Concurrently a1 a2 f) -> do
        ivar <- lift Par.new
        liftBaseDiscard Par.fork $ go a1 >>= lift . Par.put ivar
        a2' <- go a2
        a1' <- lift $ Par.get ivar
        go (f a1' a2')

      Pure a -> return a
    in go act


runIO :: Act a -> IO a
runIO act = do
  config <- loadConfig
  (a, config') <- runParIO $ runStateT (interpretParIO act) config
  saveConfig config'
  return a
