module Main where

import Gittins.Config
import Gittins.Main
import Gittins.Types

import Control.Monad.Free (Free(..))
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "register" $
    it "adds paths to the config" $
      interpret (register [] ["foo", "bar", "baz"]) emptyConfig `shouldBe`
        Config [Repository "baz" [], Repository "bar" [], Repository "foo" []]

  describe "unregister" $
    it "removes paths from the config" $
      interpret (unregister ["bar"]) (Config [Repository "foo" [], Repository "bar" []])
        `shouldBe` Config [Repository "foo" []]

emptyConfig :: Config
emptyConfig = Config []

interpret :: Act a -> Config -> Config
interpret act = case act of
  Free (Print _ a)      -> interpret a
  Free (LoadConfig f)   -> \c -> interpret (f c) c
  Free (SaveConfig c a) -> \_ -> interpret a c
  Free (Shell _ f)      -> interpret (f "")
  Pure _ -> id
