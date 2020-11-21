module GraphQL.Schema.IntrospectionTest where

import GraphQL.Client.Types
import GraphQL.Introspection (runIntrospection')
import GraphQL.Introspection.Marshalling.Types (IntrospectionResponse (..))
import GraphQL.Introspection.Schema
import GraphQL.Schema.Fixtures
import Relude
import Test.Tasty ()
import Test.Tasty.Hspec

stubbedIntrospection :: GraphQLResponse IntrospectionResponse -> (Text -> Maybe (GraphQLResponse IntrospectionResponse))
stubbedIntrospection resp = const (Just resp)

spec_introspection :: Spec
spec_introspection = do
  describe "when a successful response is provided" $ do
    it "returns schema" $ do
      runIntrospection' (stubbedIntrospection introspectionValidResponse) `shouldSatisfy` isJust

  describe "when the data can't be parsed" $ do
    it "returns an introspection error" $ do
      runIntrospection' (stubbedIntrospection introspectionInvalidResponse) `shouldSatisfy` isNothing

spec_introspectionSchema :: Spec
spec_introspectionSchema = do
  describe "queryType" $ do
    it "finds the query" $ do
      query validSchema `shouldBe` NamedType "Query"

validSchema :: Schema
validSchema = case runIntrospection' (stubbedIntrospection introspectionValidResponse) of
  (Just s) -> s
  _ -> error "Schema should be valid"
