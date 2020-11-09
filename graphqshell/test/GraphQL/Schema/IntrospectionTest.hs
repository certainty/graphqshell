{-# LANGUAGE QuasiQuotes #-}

module GraphQL.Schema.IntrospectionTest where

import Relude
import Test.Tasty ()
import Test.Tasty.Hspec
import GraphQL.Schema.Introspection
import GraphQL.Schema.Fixtures

spec_prelude :: Spec
spec_prelude = do
  describe "parse schema from introspection response" $ do
    it "returns schema when it succeeds" $ do
      schemaFromIntrospectionResponse "" `shouldSatisfy` isRight

