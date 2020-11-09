module GraphQL.Schema.IntrospectionTest where

import Relude
import Test.Tasty ()
import Test.Tasty.Hspec
import GraphQL.Schema.Introspection
import GraphQL.Schema.Fixtures (introspectionSuccessResponse)

spec_introspection :: Spec
spec_introspection = do
  describe "when a successful response is provided" $ do
    it "returns schema" $ do
      schemaFromIntrospectionResponse introspectionSuccessResponse `shouldSatisfy` isRight

