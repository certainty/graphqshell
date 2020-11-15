module GraphQL.API
  (
      API
    , mkAPI
    , introspect
    , endpointURI
  ) where
import Relude
import Control.Monad.Catch (MonadThrow)
import Control.Exception.Safe (throw)
import qualified Text.URI as URI
import GraphQL.Types
import GraphQL.Client
import GraphQL.Schema.Introspection (Schema, introspectionQuery, schemaFromIntrospectionResponse)
import qualified GraphQL.Schema.Introspection.Internal as I

data API = API {
   client :: Client
} deriving (Eq, Show)

mkAPI :: (MonadThrow m) => Text -> m API
mkAPI uri = API <$> (mkClient uri)

introspect :: (MonadThrow m, MonadIO m) => API -> m Schema
introspect api = do
  rawResponse <- introspect' api
  case schemaFromIntrospectionResponse rawResponse of
    (Left e)       -> throw e
    (Right schema) -> pure schema

introspect' :: (MonadThrow m, MonadIO m) => API -> m (GraphQLResponse I.IntrospectionResponse)
introspect' api = runRequest (client api) (GraphQLQuery introspectionQuery) noVariables

noVariables :: Maybe ()
noVariables = Nothing

endpointURI :: API -> URI.URI
endpointURI = clientEndpoint . client
