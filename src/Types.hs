{-# LANGUAGE DeriveFunctor #-}

module Types where

import Config
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.State.Lazy (StateT, get, liftIO, put, runStateT)
import Data.Text (Text)
import System.Process (CreateProcess(..), CmdSpec(RawCommand), StdStream(..), createProcess)

import qualified Data.Text.IO as T

data Act' a = Info Text a
            | LoadConfig (Config -> a)
            | SaveConfig Config a
            | Shell CreateProcess (Text -> a)
            deriving Functor

type Act a = Free Act' a

logInfo :: Text -> Act ()
logInfo msg = liftF (Info msg ())

getConfig :: Act Config
getConfig = liftF (LoadConfig id)

putConfig :: Config -> Act ()
putConfig config = liftF (SaveConfig config ())

shell :: FilePath -> FilePath -> [String] -> Act Text
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

interpretStateTIO :: Act a -> StateT Config IO a
interpretStateTIO act = case act of
  Free (Info msg a) -> do
    liftIO (T.putStrLn msg)
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
      T.hGetContents hOut
    interpretStateTIO (f out)

  Pure a -> return a

runIO :: Act a -> IO a
runIO act = do
  config       <- loadConfig
  (a, config') <- runStateT (interpretStateTIO act) config
  saveConfig config'
  return a
