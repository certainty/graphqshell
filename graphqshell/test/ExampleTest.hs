module ExampleTest where

import Relude
import Test.Tasty ()
import Test.Tasty.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hprop_sortReverse :: Property
hprop_sortReverse = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
  sort xs === sort (reverse xs)

spec_prelude :: Spec
spec_prelude = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      (viaNonEmpty head [23 ..]) `shouldBe` (Just 23 :: Maybe Int)
