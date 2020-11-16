module GraphQL.Schema.IntrospectionTest where

import Relude
import Test.Tasty ()
import Test.Tasty.Hspec
import GraphQL.Introspection.Schema.Types
import GraphQL.Schema.Fixtures 

spec_introspection :: Spec
spec_introspection = do
  describe "when a successful response is provided" $ do
    it "returns schema" $ do
      schemaFromIntrospectionResponse introspectionValidResponse `shouldSatisfy` isRight
      
  describe "when the data can't be parsed" $ do
    it "returns an introspection error" $ do
      schemaFromIntrospectionResponse introspectionInvalidResponse `shouldBe` (Left (PartialResult []))

spec_introspectionSchema :: Spec
spec_introspectionSchema = do
  describe "queryType" $ do
    it "finds the query" $ do
      (queryType validSchema) `shouldSatisfy` isJust


validSchema :: Schema
validSchema = case schemaFromIntrospectionResponse introspectionValidResponse of
  (Right s) -> s
  _ -> error "Schema should be valid"
