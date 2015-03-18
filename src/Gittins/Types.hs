{-# LANGUAGE GADTs #-}

module Gittins.Types where

import Gittins.Config
import Gittins.Pretty

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par (NFData)
import Control.Monad.Par.IO (ParIO, runParIO)
import System.IO (hGetContents)
import System.Process (CreateProcess(..), CmdSpec(RawCommand), StdStream(..), createProcess)

import qualified Control.Monad.Par.Class as Par

data Act' a where
  Log :: LogMessage -> a -> Act' a
  LoadConfig :: (Config -> a) -> Act' a
  SaveConfig :: Config -> a -> Act' a
  Process :: CreateProcess -> (String -> a) -> Act' a
  Concurrently :: (NFData b, NFData c) => Act b -> Act c -> (b -> c -> a) -> Act' a

instance Functor Act' where
  fmap f a = case a of
    Log msg a1 -> Log msg (f a1)
    LoadConfig g -> LoadConfig (f . g)
    SaveConfig c a1 -> SaveConfig c (f a1)
    Process cp g -> Process cp (f . g)
    Concurrently b1 b2 g -> Concurrently b1 b2 (fmap f . g)

type Act a = Free Act' a

putLog :: LogMessage -> Act ()
putLog msg = liftF (Log msg ())

getConfig :: Act Config
getConfig = liftF (LoadConfig id)

putConfig :: Config -> Act ()
putConfig config = liftF (SaveConfig config ())

process :: FilePath -> FilePath -> [String] -> Act String
process cwd cmd args = liftF (Process cp id) where
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

interpretParIO :: Config -> Act a -> ParIO (Config, a)
interpretParIO config act = do
  consoleLock <- liftIO $ newMVar ()
  let
    go :: Config -> Act a -> ParIO (Config, a)
    go config' act' = case act' of

      Free (Log msg a) -> do
        liftIO $ do
          takeMVar consoleLock
          putDoc (prettyLog msg)
          putStrLn ""
          putMVar consoleLock ()
        go config' a

      Free (LoadConfig f) -> go config' (f config')

      -- TODO avoid/handle concurrent config saves?
      Free (SaveConfig c a) -> go c a

      Free (Process cp f) -> do
        out <- liftIO $ do
          (_, Just hOut, _, _) <- createProcess cp
          hGetContents hOut
        go config' (f out)

      Free (Concurrently a1 a2 f) -> do
        i1 <- Par.new
        i2 <- Par.new
        Par.fork $ go config' a1 >>= Par.put i1 . snd
        Par.fork $ go config' a2 >>= Par.put i2
        -- If concurrent threads write config, the last write wins
        a1' <- Par.get i1
        (c, a2') <- Par.get i2
        go c (f a1' a2')

      Pure a -> return (config', a)
    in go config act

runIO :: Act a -> IO a
runIO act = do
  config <- loadConfig
  (config', a) <- runParIO $ interpretParIO config act
  saveConfig config'
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
