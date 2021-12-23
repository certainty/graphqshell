module Infrastructure.TuiEngine.DataGraphTest where

import Infrastructure.TuiEngine.DataGraph
import Relude
import Test.Tasty ()
import Test.Tasty.Hspec

spec_DataGraph :: Spec
spec_DataGraph = do
  describe "Entity" $ do
    describe "mkEntity" $ do
      it "creates empty entity from empty list" $ do
        isEmpty (mkEntity (Id 10) []) `shouldBe` True
      it "creates entity from list" $ do
        let entity = mkEntity (Id 10) ["a/a" .+ Value "v1", "a/b" .+ Value "v2"]
        entitySize entity `shouldBe` 2
        entity .? "a/a" `shouldBe` Just (Value "v1")
