module Main where

import Config (loadConfig)

main :: IO ()
main = loadConfig >>= print
