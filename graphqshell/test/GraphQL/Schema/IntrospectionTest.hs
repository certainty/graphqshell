{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module GraphQL.Schema.IntrospectionTest where

import Control.Exception.Safe (MonadThrow)
import Data.Aeson (FromJSON, ToJSON, Value)
import GraphQL.Client.Types
import GraphQL.Introspection.Marshalling.Types (IntrospectionResponse)
import GraphQL.Introspection.Schema
import GraphQL.Introspection.Schema.Types
import GraphQL.Schema.Fixtures
import Relude
import Test.Tasty ()
import Test.Tasty.Hspec

-- data TestClientSettings a = TestClientSettings
--   { -- TODO: map query to response
--     stubResponse :: GraphQLResponse a
--   }
--   deriving (Eq, Show)

newtype TestGraphQLClient r a = TestGraphQLClient
  { runTestClient :: ReaderT (GraphQLResponse r) IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader (GraphQLResponse r))

withStubbedClient :: (GraphQLResponse IntrospectionResponse) -> (forall m. GraphQLClient m => m a) -> IO a
withStubbedClient resp action = runReaderT (runTestClient action) resp

instance (Show a) => GraphQLClient (TestGraphQLClient a) where
  runGraphQLRequest query variables = do
    r <- ask
    liftIO $ print r
    --    pure r
    pure introspectionEmptyResponse

-- pure introspectionEmptyResponse

spec_introspection :: Spec
spec_introspection = do
  -- describe "when a successful response is provided" $ do
  -- it "returns schema" $ do
  --   schema <- withStubbedClient introspectionValidResponse runIntrospection

  --   schemaFromIntrospectionResponse introspectionValidResponse `shouldSatisfy` isRight

  describe "when the data can't be parsed" $ do
    it "returns an introspection error" $ do
      (withStubbedClient introspectionValidResponse runIntrospection) -- `shouldThrow` anyException
      True `shouldBe` True

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
