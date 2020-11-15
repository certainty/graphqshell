module GraphQL.API
  (
    API,
    mkAPI
  ) where
import Relude
import Control.Monad.Catch (MonadThrow)
import Control.Exception.Safe (throw)
import GraphQL.Types
import GraphQL.Client
import GraphQL.Schema.Introspection (Schema, introspectionQuery, makeSchema)
import qualified GraphQL.Schema.Introspection.Internal as I

data API = API {
  client :: Client
} deriving (Eq, Show)

mkAPI :: (MonadThrow m) => Text -> m API
mkAPI uri = API <$> (mkClient uri)

introspect :: (MonadThrow m, MonadIO m) => API -> m Schema
introspect api = do
  rawResponse <- introspect' api
  case makeSchema (graphQLResponseData rawResponse) of
    (Left e) -> throw e
    (Right schema) -> pure schema

introspect' :: (MonadThrow m, MonadIO m) => API -> m (GraphQLResponse I.IntrospectionResponse)
introspect' api = runRequest (client api) (GraphQLQuery introspectionQuery) Nothing
