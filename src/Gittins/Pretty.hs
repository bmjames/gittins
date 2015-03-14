module Gittins.Pretty (
    list
  , logMessage
  , summary

  -- * Re-exports from ansi-wl-pprint
  , Doc
  , putDoc
  , vcat
) where

import Text.PrettyPrint.ANSI.Leijen hiding (list)

-- | Simple one-line log message
logMessage :: String -> Doc
logMessage = text

-- | Show a list of items
list :: [String] -> Doc
list = vcat . map text

summary :: String -> String -> Doc
summary header body =
      line
  <>  cyan (text header)
  <$> text body
