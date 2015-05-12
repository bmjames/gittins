module Gittins.Pretty (
    list
  , prettyLog
  , summary
) where

import Gittins.Config
import Gittins.Process
import Gittins.Types

import Data.List (intercalate)
import Text.PrettyPrint.ANSI.Leijen hiding (list)

-- | Show a list of items
list :: [String] -> Doc
list = vcat . map text

summary :: String -> String -> String -> Doc
summary header body err =
     cyan (text header)
  <> (if null err then empty else line <> text err)
  <> (if null body then empty else line <> text body)

summariseRepo :: Repository -> Doc
summariseRepo (Repository n p _) = fillBreak 15 (cyan $ text n) <> brackets (text p)

prettyLog :: LogMessage -> Doc
prettyLog msg = case msg of
  AlreadyRegistered path -> text $ "Path [" ++ path ++ "] is already registered."
  NotRegistered path     -> text $ "Path [" ++ path ++ "] does not appear to be registered."
  NotAGitRepository path -> text $
    "Path [" ++ path ++ "] does not appear to be a Git working tree and so has not been registered."
    ++ "\nRun with -f|--force to register it anyway."
  Registering path       -> text "Registering " <> brackets (text path)
  Unregistering path     -> text $ "Unregistering [" ++ path ++ "]"
  GitOutput repo output  -> let ProcessResult _ out err = output
                            in summary (repoName repo) out err
  PullSummary unsuccessful successful ->
    let repoList = intercalate "," $ map repoName successful
    in text $ show (length repoList) ++ " repositories updated (" ++ repoList ++ ")."
  RepositoriesSummary rs -> vcat $ map summariseRepo rs
  ProcessError e         -> text e
