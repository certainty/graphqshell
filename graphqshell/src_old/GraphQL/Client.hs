{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GraphQL.Client
  ( ClientError (..),
    ClientSettings (..),
    IOGraphQLClient (..),
    runGraphQLClientIO,
  )
where

import Control.Exception.Safe
  ( MonadThrow,
    throw,
  )
import qualified Data.Aeson as J
import GraphQL.Client.Types
import Network.HTTP.Req
import Relude hiding
  ( Option,
  )
import qualified Text.URI as URI

data ClientError
  = InvalidURI URI.URI
  | DecodingError String
  deriving (Eq, Show, Generic)

instance Exception ClientError

data ClientSettings = ClientSettings
  { clientEndpoint :: URI.URI,
    customHeaders :: Maybe [(ByteString, ByteString)]
  }
  deriving (Eq, Show)

newtype IOGraphQLClient a = IOGraphQClient
  { runIOClient :: ReaderT ClientSettings IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader ClientSettings)

runGraphQLClientIO :: ClientSettings -> (forall m. GraphQLClient m => m a) -> IO a
runGraphQLClientIO settings action = runReaderT (runIOClient action) settings

instance GraphQLClient IOGraphQLClient where
  runGraphQLRequest query variables = do
    endpointURI <- asks clientEndpoint
    customHeaders <- (asks customHeaders >>= pure . (fromMaybe []))
    response <- case useURI endpointURI of
      Nothing -> throw (InvalidURI endpointURI)
      (Just (Left (httpURI, _))) ->
        runRequest' httpURI (endpointPort endpointURI) customHeaders requestBody
      (Just (Right (httpsURI, _))) ->
        runRequest' httpsURI (endpointPort endpointURI) customHeaders requestBody
    case decodeGraphQLResponse response of
      (Left e) -> throw (DecodingError e)
      (Right r) -> pure r
    where
      requestBody = GraphQLBody query variables
      endpointPort uri = case URI.uriAuthority uri of
        (Left _) -> Nothing
        (Right auth) -> fromIntegral <$> (URI.authPort auth)

runRequest' ::
  (J.ToJSON variables, MonadIO m) =>
  Url scheme ->
  Maybe Int ->
  [(ByteString, ByteString)] ->
  GraphQLBody variables ->
  m ByteString
runRequest' url customPort headers body = runReq defaultHttpConfig $ do
  resp <- req POST url (ReqBodyJson body) bsResponse requestOptions
  pure (responseBody resp)
  where
    requestOptions = requestPortOpt <> (mconcat customHeaderOpts)
    customHeaderOpts = map (\(k, v) -> header k v) headers
    requestPortOpt = fromMaybe mempty (port <$> customPort)

decodeGraphQLResponse ::
  (J.FromJSON resp) => ByteString -> Either String (GraphQLResponse resp)
decodeGraphQLResponse = J.eitherDecodeStrict
