module Pretty where

import Text.PrettyPrint.ANSI.Leijen

summary :: String -> String -> Doc
summary head body =
      line
  <>  cyan (text head)
  <$> text body
