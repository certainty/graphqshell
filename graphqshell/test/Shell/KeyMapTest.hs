module Shell.KeyMapTest where

import Relude
import Shell.KeyMap
import Test.Tasty ()
import Test.Tasty.Hspec

compileMaybe :: KeyMapConfiguration a -> Maybe (KeyMap a)
compileMaybe = compile

data MyCommands = CommandA | CommandB | CommandC deriving (Eq, Show)

spec_KeyMap :: Spec
spec_KeyMap = do
  describe "Key map configuration" $ do
    it "builds from single commands" $ do
      let keyMap = compileMaybe $ (cmd 'a' "My Command" ()) <> (cmd 'b' "My other command" ())
      isJust keyMap `shouldBe` True

    it "builds with subgroup" $ do
      let keyMap = compileMaybe $ (cmd 'a' "My Command" ()) <> (sub 'b' "My SubGroup" (cmd 'a' "SubCommand" ()) <> (cmd 'c' "Yet anolther" ()))
      isJust keyMap `shouldBe` True

    it "fails to build with duplicate entries on the same level" $ do
      (compile $ (cmd 'a' "" ()) <> (cmd 'a' "" ())) `shouldThrow` (== DuplicateKeyDefined 'a')

  describe "Find matchining command" $ do
    it "finds the match" $ do
      keyMap <- compile $ (cmd 'a' "Command A" CommandA) <> (sub 'b' "Group" (cmd 'b' "Command B" CommandB) <> (cmd 'c' "Command C" CommandC))
      (matchKey keyMap 'a') `shouldBe` (Just (Command "Command A" CommandA))

    it "returns nothing when there is no match" $ do
      keyMap <- compile $ (cmd 'a' "Command A" CommandA) <> (sub 'b' "Group" (cmd 'b' "Command B" CommandB) <> (cmd 'c' "Command C" CommandC))
      (matchKey keyMap 'n') `shouldBe` Nothing
