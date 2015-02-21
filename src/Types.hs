{-# LANGUAGE DeriveFunctor #-}

module Types where

import Config
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.State.Lazy (StateT, get, liftIO, put, runStateT)
import System.IO (hGetContents)
import System.Process (CreateProcess(..), CmdSpec(RawCommand), StdStream(..), createProcess)
import Text.PrettyPrint.ANSI.Leijen (Doc, putDoc, text)

data Act' a = Print Doc a
            | LoadConfig (Config -> a)
            | SaveConfig Config a
            | Shell CreateProcess (String -> a)
            deriving Functor

type Act a = Free Act' a

printDoc :: Doc -> Act ()
printDoc doc = liftF (Print doc ())

logInfo :: String -> Act ()
logInfo = printDoc . text

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

interpretStateTIO :: Act a -> StateT Config IO a
interpretStateTIO act = case act of
  Free (Print doc a) -> do
    liftIO (putDoc doc)
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
  saveConfig config'
  return a
