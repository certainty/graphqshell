module GraphQL.Client
  (
   Client
  , ClientError (..)
  , clientEndpoint
  , mkClient
  , runRequest
   ) where
import Relude hiding (Option)
import qualified Data.Aeson as J
import Network.HTTP.Req
import qualified Text.URI as URI
import Control.Exception.Safe (MonadThrow, throw)

import GraphQL.Client.Types

data Client = Client {
  clientEndpoint :: URI.URI
} deriving (Eq, Show)

mkClient :: (MonadThrow m) => Text -> m Client
mkClient uri = Client <$> (URI.mkURI uri)
  

data ClientError = InvalidURI URI.URI
                 | DecodingError String  
                deriving (Eq, Show, Generic)

instance Exception ClientError

runRequest :: (J.ToJSON variables, J.FromJSON resp, MonadIO m, MonadThrow m) => Client -> GraphQLQuery -> Maybe variables -> m (GraphQLResponse resp)
runRequest client query variables = do 
  response <- case useURI endpointURI of
                Nothing                       -> throw (InvalidURI endpointURI)
                (Just (Left  (httpURI, _) ))  -> runRequest' httpURI requestBody
                (Just (Right (httpsURI, _) )) -> runRequest' httpsURI requestBody
  case decodeGraphQLResponse response of
    (Left e)  -> throw (DecodingError e)
    (Right r) -> pure r
    
  where
    endpointURI = (clientEndpoint client)
    requestBody = GraphQLBody query variables
    
-- | Low level request 
runRequest' :: (J.ToJSON variables, MonadIO m) => (Url scheme) -> GraphQLBody variables -> m ByteString 
runRequest' url body = runReq defaultHttpConfig $ do
  resp <- req POST url  (ReqBodyJson body) bsResponse requestOptions 
  pure (responseBody resp)
  where
    requestOptions = mempty

decodeGraphQLResponse :: (J.FromJSON resp) => ByteString -> Either String (GraphQLResponse resp)
decodeGraphQLResponse = J.eitherDecodeStrict

