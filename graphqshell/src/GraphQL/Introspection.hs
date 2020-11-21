module GraphQL.Introspection
  ( runIntrospection,
    runIntrospection',
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import GraphQL.Client.Types
import GraphQL.Introspection.Marshalling.Types
import GraphQL.Introspection.Schema (fromMarshalledSchema)
import GraphQL.Introspection.Schema.Types hiding (deprecationReason, isDeprecated, name)
import Relude

runIntrospection :: (GraphQLClient m) => m Schema
runIntrospection = runIntrospection' (\t -> runGraphQLRequest (GraphQLQuery t) emptyVariables)

runIntrospection' :: (MonadThrow m) => (Text -> m (GraphQLResponse IntrospectionResponse)) -> m Schema
runIntrospection' f = do
  response <- f introspectionQuery
  case response of
    (GraphQLResponse (Just (IntrospectionResponse schema)) _) -> throwLeft (fromMarshalledSchema schema)
    (GraphQLResponse _ (Just errors)) -> throw (PartialResult errors)
    _ -> throw EmptyGraphQLReponse
  where
    throwLeft (Left e) = throw e
    throwLeft (Right r) = pure r
