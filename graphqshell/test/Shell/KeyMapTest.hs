module Shell.KeyMapTest where

import Relude
import qualified Relude.Unsafe as Unsafe
import Shell.KeyMap
import Test.Tasty ()
import Test.Tasty.Hspec

compileMaybe :: KeyMapConfiguration a -> Maybe (KeyMap a)
compileMaybe = compile

spec_KeyMap :: Spec
spec_KeyMap = do
  describe "Key map confighuration" $ do
    it "builds correctly" $ do
      let keyMap = compileMaybe $ (cmd 'a' "My Command" ()) <> (cmd 'b' "My other command" ())
      isJust keyMap `shouldBe` True
