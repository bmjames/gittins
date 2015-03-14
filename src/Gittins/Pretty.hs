module Gittins.Pretty (
    list
  , logMessage
  , summary

  -- * Re-exports from ansi-wl-pprint
  , Doc
  , putDoc
) where

import Text.PrettyPrint.ANSI.Leijen hiding (list)

logMessage :: String -> Doc
logMessage = text

list :: [String] -> Doc
list = vcat . map text

summary :: String -> String -> Doc
summary head body =
      line
  <>  cyan (text head)
  <$> text body
