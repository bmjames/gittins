{-# LANGUAGE ScopedTypeVariables #-}

module Gittins.FilePath where

import Control.Exception
import System.Directory (canonicalizePath)

-- | A version of 'canonicalizePath' which won't throw an exception if the
--   directory doesn't exist
safeCanonicalize :: FilePath -> IO FilePath
safeCanonicalize p = canonicalizePath p `catchIO` (\_ -> return p)

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch
