module Shell.Components.UtilitiesTest where

import           Relude
import           Test.Tasty                     ( )
import           Test.Tasty.Hspec

-- | TODO: figure out how to run the event monad in tests
spec_ComponentUtilities :: Spec
spec_ComponentUtilities = do
  describe "updateComponent" $ do
    context "when component's update returns halt" $ do
      it "returns halt" $ do
        True `shouldBe` True
