{-# LANGUAGE DeriveFunctor #-}

module Types where

import Config
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.State.Lazy (StateT, get, liftIO, put, runStateT)

data Act' a = Info String a
            | LoadConfig (Config -> a)
            | SaveConfig Config a
            deriving Functor

type Act a = Free Act' a

logInfo :: String -> Act ()
logInfo msg = liftF (Info msg ())

getConfig :: Act Config
getConfig = liftF (LoadConfig id)

putConfig :: Config -> Act ()
putConfig config = liftF (SaveConfig config ())

interpretStateTIO :: Act a -> StateT Config IO a
interpretStateTIO act = case act of
  Free (Info msg a)     -> do liftIO (putStrLn msg)
                              interpretStateTIO a
  Free (LoadConfig f)   -> do config <- get
                              interpretStateTIO (f config)
  Free (SaveConfig c a) -> do put c
                              interpretStateTIO a
  Pure a                -> return a

runIO :: Act a -> IO a
runIO act = do
  config       <- loadConfig
  (a, config') <- runStateT (interpretStateTIO act) config
  saveConfig config'
  return a
