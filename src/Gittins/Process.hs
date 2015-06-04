module Gittins.Process where

import System.Exit (ExitCode)
import System.IO (hGetContents)
import System.Process (CreateProcess(..), CmdSpec(RawCommand), StdStream(..),
                       createProcess, waitForProcess)

data ProcessResult = ProcessResult ExitCode String String
                    deriving (Eq, Ord, Show)

processResult :: FilePath -> FilePath -> [String] -> IO ProcessResult
processResult workingDir cmd args = do
  (_, Just hOut, Just hErr, ph) <- createProcess $ mkProcess workingDir cmd args
  out <- hGetContents hOut
  err <- hGetContents hErr
  exitCode <- waitForProcess ph
  return $ ProcessResult exitCode out err

mkProcess :: FilePath -> FilePath -> [String] -> CreateProcess
mkProcess workingDir cmd args =
  CreateProcess { cmdspec = RawCommand cmd args
                , cwd = Just workingDir
                , env = Nothing
                , std_in = Inherit
                , std_out = CreatePipe
                , std_err = CreatePipe
                , close_fds = False
                , create_group = False
                , delegate_ctlc = False
                }
