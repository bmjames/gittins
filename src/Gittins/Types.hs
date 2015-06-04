{-# LANGUAGE GADTs #-}

module Gittins.Types where

import Gittins.Config
import Gittins.Process

import Control.Monad (void)
import Control.Monad.Free (Free(..), liftF)

data Act' a where
  Log :: LogMessage -> a -> Act' a
  LoadConfig :: (Config -> a) -> Act' a
  SaveConfig :: Config -> a -> Act' a
  Process :: FilePath -> FilePath -> [String] -> (ProcessResult -> a) -> Act' a
  IsWorkingTree :: FilePath -> (Bool -> a) -> Act' a
  Concurrently :: Act b -> Act c -> (b -> c -> a) -> Act' a

instance Functor Act' where
  fmap f a = case a of
    Log msg a1 -> Log msg (f a1)
    LoadConfig g -> LoadConfig (f . g)
    SaveConfig c a1 -> SaveConfig c (f a1)
    Process wd cmd args g -> Process wd cmd args (f . g)
    IsWorkingTree p g -> IsWorkingTree p (f . g)
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

-- | Determine whether the given file path appears to refer to
--   the root of a valid Git working tree.
isWorkingTree :: FilePath -> Act Bool
isWorkingTree path = liftF (IsWorkingTree path id)

concurrently :: Act a -> Act b -> Act (a, b)
concurrently a1 a2 = liftF (Concurrently a1 a2 (,))

concurrentSeq :: [Act a] -> Act [a]
concurrentSeq = foldr (fmap (fmap (uncurry (:))) . concurrently) (return [])

concurrentSeq_ :: [Act ()] -> Act ()
concurrentSeq_ = foldr (fmap void . concurrently) (return ())

concurrentFor :: [a] -> (a -> Act b) -> Act [b]
concurrentFor as f = concurrentSeq $ map f as

concurrentFor_ :: [a] -> (a -> Act ()) -> Act ()
concurrentFor_ as f = concurrentSeq_ $ map f as

getReposForGroup :: [GroupId] -> Act [Repository]
getReposForGroup groupIds = do
  Config repos <- getConfig
  return $ filter (\(Repository _ _ gs) -> null groupIds || any (`elem` groupIds) gs) repos

data LogMessage = AlreadyRegistered FilePath
                | NotRegistered FilePath
                | NotAGitRepository FilePath
                | Registering FilePath
                | Unregistering FilePath
                | RepositoriesSummary [Repository]
                | GitOutput Repository ProcessResult
                | PullSummary [Repository] [Repository]
                | StatusSummary [(Repository, ProcessResult)]
                | DiffSummary [(Repository, ProcessResult)]
                | ProcessError String
                deriving (Eq, Ord, Show)
