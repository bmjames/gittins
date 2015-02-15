module Main where

import Config
import Data.Foldable (foldlM)
import Options.Applicative
import System.Directory (canonicalizePath)

data Opts = Register [FilePath]
          | Unregister [FilePath]
          deriving (Eq, Ord, Show)

parseOpts :: Parser Opts
parseOpts = subparser $
     command "register" (info registerOpts (progDesc "Register one or more repositories"))
  <> command "unregister" (info unregisterOpts (progDesc "Unregister one ore more repositories"))

  where
    registerOpts = Register <$> paths
    unregisterOpts = Unregister <$> paths
    paths =  many (strArgument (metavar "PATH"))

-- | Entry point for register command
register :: [FilePath] -> IO ()
register paths = modifyConfig $ \config -> foldlM addRepo config paths

  where
    addRepo :: Config -> FilePath -> IO Config
    addRepo config@(Config repos) p = do
      canonical <- canonicalizePath p
      if any (\r -> path r == canonical) repos
        then do putStrLn ("Path [" ++ canonical ++ "] is already registered.")
                return config
        else do putStrLn ("Registering [" ++ canonical ++ "]")
                let repo = Repository canonical
                return $ Config (repo : repos)

-- | Entry point for unregister command
unregister :: [FilePath] -> IO ()
unregister paths = modifyConfig $ \config -> foldlM rmRepo config paths

  where
    rmRepo :: Config -> FilePath -> IO Config
    rmRepo config@(Config repos) p = do
      canonical <- canonicalizePath p
      if not $ any (\r -> path r == canonical) repos
         then do putStrLn ("Path [" ++ canonical ++ "] does not appear to be registered.")
                 return config
         else do putStrLn ("Unregistering [" ++ canonical ++ "]")
                 let repos' = filter (\r -> path r /= canonical) repos
                 return $ Config repos'

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOpts) fullDesc
  case opts of
    Register paths   -> register paths
    Unregister paths -> unregister paths
