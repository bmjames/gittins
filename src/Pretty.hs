module Pretty where

import Text.PrettyPrint.ANSI.Leijen

list :: [String] -> Doc
list = vcat . map text

summary :: String -> String -> Doc
summary head body =
      line
  <>  cyan (text head)
  <$> text body
