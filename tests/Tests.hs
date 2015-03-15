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
      interpret (register [] ["baz", "qux"]) initConfig `shouldBe`
        Config [Repository p [] | p <- ["qux", "baz", "foo", "bar"]]

  describe "unregister" $
    it "removes paths from the config" $
      interpret (unregister ["bar"]) initConfig `shouldBe`
        Config [Repository "foo" []]

  describe "add-to-group" $
    it "adds repositories to a group" $
      interpret (addToGroup ["my-group"] ["foo", "bar"]) initConfig `shouldBe`
        Config [Repository p ["my-group"] | p <- ["foo", "bar"]]

  describe "remove-from-group" $
    it "removes repositories from a group" $
      interpret (removeFromGroup ["my-group"] ["foo", "bar"])
                (Config [Repository p ["my-group"] | p <- ["foo", "bar"]])
        `shouldBe` initConfig

initConfig :: Config
initConfig = Config [Repository "foo" [], Repository "bar" []]

interpret :: Act a -> Config -> Config
interpret act = case act of
  Free (Log _ a)        -> interpret a
  Free (LoadConfig f)   -> \c -> interpret (f c) c
  Free (SaveConfig c a) -> \_ -> interpret a c
  Free (Shell _ f)      -> interpret (f "")
  Pure _ -> id
