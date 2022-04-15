{-# LANGUAGE RankNTypes #-}

module GQShell.Infrastructure.GraphQL.IOApi
  ( IOApi,
    runApiIO,
    mkApiSettings,
    apiURI,
  )
where

import Control.Exception.Safe (MonadThrow)
import GQShell.Application.Config (EndpointConfig, endpointHttpConfig, endpointHttpHeaders)
import GQShell.Application.Config.Types (endpointURL)
import GQShell.Core.GraphQL.API
import GQShell.Core.GraphQL.Introspection (runIntrospection)
import GQShell.Infrastructure.GraphQL.IOClient (ClientSettings (..), runGraphQLClientIO)
import Lens.Micro.Platform ((^.), (^?), _Just)
import Relude
import qualified Text.URI as URI

newtype ApiSettings = ApiSettings
  { clientSettings :: ClientSettings
  }
  deriving (Eq, Show)

mkApiSettings :: EndpointConfig -> ApiSettings
mkApiSettings config =
  ApiSettings $
    ClientSettings
      (config ^. endpointURL)
      (config ^? endpointHttpConfig . _Just . endpointHttpHeaders . _Just)

apiURI :: ApiSettings -> URI.URI
apiURI = clientEndpoint . clientSettings

-- Move to infrastructure
newtype IOApi a = IOApi
  { runIOAPI :: ReaderT ApiSettings IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader ApiSettings)

instance GraphQLApi IOApi where
  introspect = do
    settings <- asks clientSettings
    liftIO $ runGraphQLClientIO settings runIntrospection

runApiIO :: ApiSettings -> (forall m. GraphQLApi m => m a) -> IO a
runApiIO settings action = runReaderT (runIOAPI action) settings
