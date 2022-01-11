module GraphQL.Introspection
  ( IntrospectionError (..),
    runIntrospection,
    runIntrospection',
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import GraphQL.Client.Types
import GraphQL.Introspection.Marshalling.Types
import GraphQL.Introspection.Schema (Schema, fromIntrospectionSchema)
import Relude

data IntrospectionError
  = IntrospectionError Text
  | UnexpectedGraphQLResponse
  deriving (Eq, Show)

instance Exception IntrospectionError

runIntrospection :: (GraphQLClient m) => m Schema
runIntrospection = runIntrospection' (`runGraphQLRequest` emptyVariables)

-- | This function is really only used to make this testable
--   Please refer to 'runIntrospection' instead for the monadic interface that is encouraged to be used
runIntrospection' :: (MonadThrow m) => (GraphQLQuery -> m (GraphQLResponse IntrospectionResponse)) -> m Schema
runIntrospection' runQuery = do
  response <- runQuery (GraphQLQuery introspectionQuery)
  case response of
    (SuccessResponse (IntrospectionResponse schema)) -> fromIntrospectionSchema schema
    _ -> throw UnexpectedGraphQLResponse
