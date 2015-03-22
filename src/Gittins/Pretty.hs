module Gittins.Pretty (
    list
  , logMessage
  , prettyLog
  , summary
) where

import Gittins.Config
import Gittins.Process
import Gittins.Types

import Text.PrettyPrint.ANSI.Leijen hiding (list)

-- | Simple one-line log message
logMessage :: String -> Doc
logMessage = text

-- | Show a list of items
list :: [String] -> Doc
list = vcat . map text

summary :: String -> String -> String -> Doc
summary header body err =
     cyan (text header)
  <> (if null err then empty else line <> text err)
  <> (if null body then empty else line <> text body)

summariseRepo :: Repository -> Doc
summariseRepo (Repository n p _) = fillBreak 10 (cyan $ text n) <> brackets (text p)

prettyLog :: LogMessage -> Doc
prettyLog msg = case msg of
  AlreadyRegistered path -> logMessage $ "Path [" ++ path ++ "] is already registered."
  NotRegistered path     -> logMessage $ "Path [" ++ path ++ "] does not appear to be registered."
  NotAGitRepository path -> logMessage $
    "Path [" ++ path ++ "] does not appear to be a Git working tree and so has not been registered."
    ++ "\nRun with -f|--force to register it anyway."
  Registering path       -> text "Registering " <> brackets (text path)
  Unregistering path     -> logMessage $ "Unregistering [" ++ path ++ "]"
  PullSummary rs         -> vcat $ map (\(r, ProcessResult _ out err) -> summary (repoName r) out err) rs
  StatusSummary rs       -> vcat $ map (\(r, ProcessResult _ out err) -> summary (repoName r) out err) rs
  RepositoriesSummary rs -> vcat $ map summariseRepo rs
  ProcessError e         -> logMessage e
