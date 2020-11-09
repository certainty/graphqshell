module GraphQL.Schema.IntrospectionTest where

import Relude
import Test.Tasty ()
import Test.Tasty.Hspec
import GraphQL.Schema.Introspection
import GraphQL.Schema.Fixtures 

spec_introspection :: Spec
spec_introspection = do
  describe "when a successful response is provided" $ do
    it "returns schema" $ do
      schemaFromIntrospectionResponse introspectionValidResponse `shouldSatisfy` isRight
      
  describe "when the data can't be parsed" $ do
    it "returns an introspection error" $ do
      schemaFromIntrospectionResponse introspectionInvalidResponse `shouldBe` (Left (IntrospectionError "Error in $.schema.queryType: parsing GraphQL.Schema.Introspection.Internal.RootTypeName(RootTypeName) failed, expected Object, but encountered Null"))

