{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GraphQL.API
  ( ApiSettings
  , mkApiSettings
  , IOApi(..)
  , runApiIO
  , introspect
  , apiURI
  )
where

import           Control.Exception.Safe         ( MonadThrow )
import           GraphQL.Api.Types              ( GraphQLApi(..) )
import           GraphQL.Client                 ( ClientSettings(..)
                                                , runGraphQLClientIO
                                                )
import           GraphQL.Introspection          ( runIntrospection )
import           Relude
import qualified Text.URI                      as URI
import           Shell.Configuration
import           Lens.Micro.Platform            ( (^.)
                                                , (^?)
                                                , _Just
                                                )

newtype ApiSettings = ApiSettings
  { clientSettings :: ClientSettings
  }
  deriving (Eq, Show)

mkApiSettings :: EndpointConfig -> ApiSettings
mkApiSettings config = ApiSettings $ ClientSettings
  (config ^. endpointURL)
  (config ^? endpointHttpConfig . _Just . endpointHttpHeaders . _Just)

apiURI :: ApiSettings -> URI.URI
apiURI = clientEndpoint . clientSettings

newtype IOApi a = IOApi
  { runIOAPI :: ReaderT ApiSettings IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader ApiSettings)

instance GraphQLApi IOApi where
  introspect = do
    settings <- asks clientSettings
    liftIO $ runGraphQLClientIO settings runIntrospection

runApiIO :: ApiSettings -> (forall m . GraphQLApi m => m a) -> IO a
runApiIO settings action = runReaderT (runIOAPI action) settings
