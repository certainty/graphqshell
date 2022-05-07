module Hubble.KeyMapSpec where

import qualified Graphics.Vty as Vty
import Hubble.KeyMap
    ( matches,
      mkBinding,
      withHelp,
      withoutHelp,
      Binding(_bindingKeys),
      BindingState(Disabled, Enabled),
      Key(Key) )
import Relude ( ($), Eq, Ord, Bool(False, True), (<$>) )
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Test.Tasty ()
import qualified Test.Tasty.Hspec ()

data TestId = CtrlUp deriving (Ord, Eq)

spec_keyMap :: Spec
spec_keyMap = do
  describe "mkBinding" $ do
    it "recognizes sole characters" $ do
      let b = mkBinding Enabled ["a"] (withHelp "a" "just an a")
      _bindingKeys <$> b `shouldReturn` [Key (Vty.KChar 'a', [])]

    it "recognizes multiple keys" $ do
      let b = mkBinding Enabled ["a", "b"] withoutHelp
      _bindingKeys <$> b `shouldReturn` [Key (Vty.KChar 'a', []), Key (Vty.KChar 'b', [])]

    it "recognises modifiers" $ do
      let b = mkBinding Enabled ["C-a"] withoutHelp
      _bindingKeys <$> b `shouldReturn` [Key (Vty.KChar 'a', [Vty.MCtrl])]

    it "recognizes multiple modifiers" $ do
      let b = mkBinding Enabled ["C-M-a"] withoutHelp
      _bindingKeys <$> b `shouldReturn` [Key (Vty.KChar 'a', [Vty.MCtrl, Vty.MMeta])]

    it "recognises symbolic keys" $ do
      let b = mkBinding Enabled ["<up>"] withoutHelp
      _bindingKeys <$> b `shouldReturn` [Key (Vty.KUp, [])]

  describe "matches" $ do
    it "returns true if the key matches the binding" $ do
      b <- mkBinding Enabled ["C-a"] withoutHelp
      b `matches` Key (Vty.KChar 'a', [Vty.MCtrl]) `shouldBe` True
    it "returns false if the key does not match the binding" $ do
      b <- mkBinding Enabled ["C-a"] withoutHelp
      (b `matches` Key (Vty.KChar 'b', [Vty.MCtrl])) `shouldBe` False

    it "returns false if the key is disabled even if the chord matches" $ do
      b <- mkBinding Disabled ["C-a"] withoutHelp
      b `matches` Key (Vty.KChar 'a', [Vty.MCtrl]) `shouldBe` False
