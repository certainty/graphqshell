module GraphQL.Schema.IntrospectionTest where

import qualified Data.Vector as Vector
import GraphQL.Client.Types
import GraphQL.Introspection (runIntrospection')
import GraphQL.Introspection.Marshalling.Types (IntrospectionResponse)
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
    it "returns schema" $
      runIntrospection' (stubbedIntrospection introspectionValidResponse) `shouldSatisfy` isRight

  describe "when the data can't be parsed" $
    it "returns an introspection error" $
      runIntrospection' (stubbedIntrospection introspectionInvalidResponse) `shouldSatisfy` isLeft

spec_introspectionSchema :: Spec
spec_introspectionSchema = do
  describe "queryType" $
    it "finds the query" $
      name (Schema.query validSchema) `shouldBe` "Query"

  describe "lookupType" $ do
    it "finds the named type" $
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
    it "finds types matching the beginning" $
      doSearch "Clou" validSchema `shouldBe` [NamedType "Clouds"]

    it "finds types matching something in the middle" $
      doSearch "oud" validSchema `shouldBe` [NamedType "Clouds"]

    it "finds multiple matches" $
      doSearch "Foot" testSchema `shouldBe` [NamedType "FootLocker", NamedType "Football"]

    it "finds matches even for short search strings" $
      doSearch "Fo" testSchema `shouldBe` [NamedType "FootLocker", NamedType "Football"]

    it "finds matches for substrings" $
      doSearch "Foal" testSchema `shouldBe` [NamedType "Football"]

    it "is case insensitive" $ do
      doSearch "foOt" testSchema `shouldBe` [NamedType "FootLocker", NamedType "Football"]

    it "finds nothing if there is nothing" $ do
      doSearch "nope" testSchema `shouldBe` []
  where
    doSearch needle schema = namedType <$> Schema.searchType needle surround schema
    namedType (tpe, _, _) = tpe
    surround :: (Text, Text)
    surround = ("", "")

testSchema :: Schema.Schema
testSchema = Schema.mkSchema query Nothing Nothing otherTypes
  where
    query = ObjectType "Query" Nothing Vector.empty Vector.empty
    otherTypes =
      [ Object (ObjectType "Football" Nothing Vector.empty Vector.empty),
        Object (ObjectType "FootLocker" Nothing Vector.empty Vector.empty)
      ]

validSchema :: Schema.Schema
validSchema = case runIntrospection' (stubbedIntrospection introspectionValidResponse) of
  (Right s) -> s
  (Left e) -> error ("Schema should be valid ... " <> show e)
