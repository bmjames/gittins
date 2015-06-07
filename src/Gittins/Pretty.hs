module Gittins.Pretty (
    prettyLog
  , summary
) where

import Prelude hiding ((<$>))

import Gittins.Config
import Gittins.Process
import Gittins.Types

import Text.PrettyPrint.ANSI.Leijen hiding (list)

summary :: String -> String -> String -> Doc
summary header body err =
     cyan (text header)
  <> (if null err then empty else line <> text err)
  <> (if null body then empty else line <> text body)

summariseRepo :: Repository -> Doc
summariseRepo (Repository n p _) = fillBreak 15 (cyan $ text n) <> brackets (text p)

prettyLog :: LogMessage -> Doc
prettyLog msg = (<> line) $ case msg of
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
    let listRepos rs = case rs of
                        [] -> Nothing
                        _  -> Just $ list' $ map (text . repoName) rs
    in int (length successful) <+> text "repositories updated" <+?> listRepos successful <> dot <$?>
       fmap (\s -> int (length unsuccessful) <+> text "repositories could not be updated" <+> s <> dot)
            (listRepos unsuccessful)
  RepositoriesSummary rs -> vcat $ map summariseRepo rs
  ProcessError e         -> text e

list' :: [Doc] -> Doc
list' = encloseSep lparen rparen comma

infix 7 <+?>
(<+?>) :: Doc -> Maybe Doc -> Doc
l <+?> Just r = l <+> r
l <+?> Nothing = l

infix 7 <$?>
(<$?>) :: Doc -> Maybe Doc -> Doc
l <$?> Just r = l <$> r
l <$?> Nothing = l
