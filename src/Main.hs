module Main where

import Config (modifyConfig)
import Options.Applicative

data Opts = Register [FilePath]
          | Unregister [FilePath]
          deriving (Eq, Ord, Show)

parseOpts :: Parser Opts
parseOpts = subparser $
     command "register" (info register (progDesc "Register one or more repositories"))
  <> command "unregister" (info unregister (progDesc "Unregister one ore more repositories"))

  where
    register = Register <$> paths
    unregister = Unregister <$> paths
    paths =  many (strArgument (metavar "PATH"))

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOpts) fullDesc
  print opts
  modifyConfig $ \config -> print config >> return config
