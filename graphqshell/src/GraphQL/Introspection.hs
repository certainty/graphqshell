module GraphQL.Introspection
  ( IntrospectionError (..),
    runIntrospection,
    runIntrospection',
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import GraphQL.Client.Types
import GraphQL.Introspection.Marshalling.Types
import GraphQL.Introspection.Schema (fromMarshalledSchema)
import GraphQL.Introspection.Schema.Types hiding (deprecationReason, isDeprecated, name)
import Relude

data IntrospectionError
  = IntrospectionError Text
  | PartialResult [GraphQLError]
  deriving (Eq, Show)

instance Exception IntrospectionError

runIntrospection :: (GraphQLClient m) => m Schema
runIntrospection = runIntrospection' (\t -> runGraphQLRequest (GraphQLQuery t) emptyVariables)

-- | This function is really only used to make this testable
--   Please refer to 'runIntrospection' instead for the monadic interface that is encouraged to be used
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
