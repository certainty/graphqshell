{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GraphQL.Schema.IntrospectionTest where

import Control.Exception.Safe (MonadThrow)
import GraphQL.Client.Types
import GraphQL.Introspection.Schema
import GraphQL.Introspection.Schema.Types
import GraphQL.Schema.Fixtures
import Relude
import Test.Tasty ()
import Test.Tasty.Hspec

data TestClientSettings a = TestClientSettings
  { -- TODO: map query to response
    stubResponse :: GraphQLResponse a
  }
  deriving (Eq, Show)

newtype TestGraphQLClient b a = TestGraphQLClient
  { runTestClient :: ReaderT (TestClientSettings b) IO a
  }
  deriving (Functor, Applicative, Monad, MonadThrow, MonadIO, MonadReader (TestClientSettings b))

instance GraphQLClient (TestGraphQLClient resp) where
  runGraphQLRequest _query _variables = do
    r <- asks stubResponse
    pure r

withStubbedClient :: GraphQLResponse a -> (forall m. GraphQLClient m => m b) -> IO b
withStubbedClient resp action = runReaderT (runTestClient action) (TestClientSettings resp)

spec_introspection :: Spec
spec_introspection = do
  -- describe "when a successful response is provided" $ do
  -- it "returns schema" $ do
  --   schema <- withStubbedClient introspectionValidResponse runIntrospection

  --   schemaFromIntrospectionResponse introspectionValidResponse `shouldSatisfy` isRight

  describe "when the data can't be parsed" $ do
    it "returns an introspection error" $ do
      (withStubbedClient introspectionEmptyResponse runIntrospection) `shouldThrow` anyException

-- schemaFromIntrospectionResponse introspectionInvalidResponse `shouldBe` (Left (PartialResult []))

-- spec_introspectionSchema :: Spec
-- spec_introspectionSchema = do
--   describe "queryType" $ do
--     it "finds the query" $ do
--       (queryType validSchema) `shouldSatisfy` isJust

-- validSchema :: Schema
-- validSchema = case schemaFromIntrospectionResponse introspectionValidResponse of
--   (Right s) -> s
--   _ -> error "Schema should be valid"
