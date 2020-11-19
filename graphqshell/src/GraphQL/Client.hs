{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GraphQL.Client
  ( ClientError (..),
    ClientSettings (..),
    IOGraphQLClient (..),
    runGraphQLClientIO,
  )
where

import Control.Exception.Safe (MonadThrow, throw)
import qualified Data.Aeson as J
import GraphQL.Client.Types
import Network.HTTP.Req
import Relude hiding (Option)
import qualified Text.URI as URI

data ClientError
  = InvalidURI URI.URI
  | DecodingError String
  deriving (Eq, Show, Generic)

instance Exception ClientError

data ClientSettings = ClientSettings
  { clientEndpoint :: URI.URI
  }
  deriving (Eq, Show)

newtype IOGraphQLClient a = IOGraphQClient
  { runIOClient :: ReaderT ClientSettings IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader ClientSettings)

runGraphQLClientIO :: ClientSettings -> (forall m. GraphQLClient m => m a) -> IO a
runGraphQLClientIO settings action = runReaderT (runIOClient action) settings

instance GraphQLClient IOGraphQLClient where
  runGraphQLRequest :: (J.ToJSON variables, J.FromJSON resp, MonadIO m, MonadThrow m, MonadReader ClientSettings m) => GraphQLQuery -> Maybe variables -> m (GraphQLResponse resp)
  runGraphQLRequest query variables = do
    endpointURI <- asks clientEndpoint
    response <- case useURI endpointURI of
      Nothing -> throw (InvalidURI endpointURI)
      (Just (Left (httpURI, _))) -> runRequest' httpURI requestBody
      (Just (Right (httpsURI, _))) -> runRequest' httpsURI requestBody
    case decodeGraphQLResponse response of
      (Left e) -> throw (DecodingError e)
      (Right r) -> pure r
    where
      requestBody = GraphQLBody query variables

runRequest' :: (J.ToJSON variables, MonadIO m) => (Url scheme) -> GraphQLBody variables -> m ByteString
runRequest' url body = runReq defaultHttpConfig $ do
  resp <- req POST url (ReqBodyJson body) bsResponse requestOptions
  pure (responseBody resp)
  where
    requestOptions = mempty

decodeGraphQLResponse :: (J.FromJSON resp) => ByteString -> Either String (GraphQLResponse resp)
decodeGraphQLResponse = J.eitherDecodeStrict
