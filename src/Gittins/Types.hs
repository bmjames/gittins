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

concurrentSeq_ :: NFData a => [Act a] -> Act ()
concurrentSeq_ = foldr (fmap void . concurrently) (return ())

getReposForGroup :: [GroupId] -> Act [Repository]
getReposForGroup groupIds = do
  Config repos <- getConfig
  return $ filter (\(Repository _ gs) -> null groupIds || any (`elem` groupIds) gs) repos

interpretParIO :: Config -> Act a -> ParIO a
interpretParIO config act' = do
  consoleLock <- liftIO $ newMVar ()
  let
    go :: Act a -> ParIO a
    go act = case act of

      Free (Log msg a) -> do
        liftIO $ do
          takeMVar consoleLock
          putDoc (prettyLog msg)
          putStrLn ""
          putMVar consoleLock ()
        go a

      Free (LoadConfig f) -> go (f config)

      -- TODO avoid/handle concurrent config saves?
      Free (SaveConfig _ a) -> go a

      Free (Process cp f) -> do
        out <- liftIO $ do
          (_, Just hOut, _, _) <- createProcess cp
          hGetContents hOut
        go (f out)

      Free (Concurrently a1 a2 f) -> do
        i1 <- Par.new
        i2 <- Par.new
        Par.fork $ go a1 >>= Par.put i1
        Par.fork $ go a2 >>= Par.put i2
        a1' <- Par.get i1
        a2' <- Par.get i2
        go (f a1' a2')

      Pure a -> return a
    in go act'

runIO :: Act a -> IO a
runIO act = do
  config <- loadConfig
  runParIO $ interpretParIO config act

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
