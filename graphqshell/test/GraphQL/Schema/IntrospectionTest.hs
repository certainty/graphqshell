module GraphQL.Schema.IntrospectionTest where

import GraphQL.Client.Types
import GraphQL.Introspection (runIntrospection')
import GraphQL.Introspection.Marshalling.Types (IntrospectionResponse (..))
import qualified GraphQL.Introspection.Schema as Schema
import GraphQL.Introspection.Schema.Types
import GraphQL.Schema.Fixtures
import Relude
import Test.Tasty ()
import Test.Tasty.Hspec

stubbedIntrospection :: GraphQLResponse IntrospectionResponse -> (GraphQLQuery -> Either e (GraphQLResponse IntrospectionResponse))
stubbedIntrospection resp = const (Right resp)

spec_introspection :: Spec
spec_introspection = do
  describe "when a successful response is provided" $
    it "returns schema" $ do
      runIntrospection' (stubbedIntrospection introspectionValidResponse) `shouldSatisfy` isRight

  describe "when the data can't be parsed" $
    it "returns an introspection error" $ do
      runIntrospection' (stubbedIntrospection introspectionInvalidResponse) `shouldSatisfy` isLeft

spec_introspectionSchema :: Spec
spec_introspectionSchema = do
  describe "queryType" $
    it "finds the query" $ do
      Schema.query validSchema `shouldBe` NamedType "Query"

  describe "lookupType" $ do
    it "finds the named type" $ do
      (name <$> Schema.lookupType (NamedType "City") validSchema) `shouldBe` Just "City"

    it "finds type that's wrapped in List" $
      (name <$> Schema.lookupType (ListOf (NamedType "City")) validSchema) `shouldBe` Just "City"

    it "finds type that's wrapped in NonNull" $
      (name <$> Schema.lookupType (NonNullOf (NamedType "City")) validSchema) `shouldBe` Just "City"

    it "finds type that's wrapped in combination of NonNull and ListOf" $
      (name <$> Schema.lookupType (NonNullOf (ListOf (NonNullOf (NamedType "City")))) validSchema) `shouldBe` Just "City"

    it "returns nothing for non-named type" $
      Schema.lookupType UnnamedType validSchema `shouldBe` Nothing

    it "returns nothing when type can't be found" $
      Schema.lookupType (NamedType "NonExistent") validSchema `shouldBe` Nothing

  describe "searchType" $ do
    it "finds types matching the beginning" $ do
      (snd <$> Schema.searchType "Clou" validSchema) `shouldBe` [NamedType "Clouds"]

    it "finds types matching something in the middle" $ do
      (snd <$> Schema.searchType "oud" validSchema) `shouldBe` [NamedType "Clouds"]

    it "finds types when single letter is given" $ do
      (snd <$> Schema.searchType "W" validSchema) `shouldBe` [NamedType "City"]

validSchema :: Schema
validSchema = case runIntrospection' (stubbedIntrospection introspectionValidResponse) of
  (Right s) -> s
  (Left e) -> error ("Schema should be valid ... " <> (show e))
