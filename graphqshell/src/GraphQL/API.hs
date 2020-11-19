module GraphQL.API where

import Control.Exception.Safe (MonadThrow)
import GraphQL.Api.Types (GraphQLApi (..))
import GraphQL.Client (ClientSettings, runGraphQLClientIO)
import GraphQL.Introspection.Schema (runIntrospection)
import Relude

data ApiSettings = ApiSettings
  { clientSettings :: ClientSettings
  }
  deriving (Eq, Show)

newtype IOApi a = IOApi
  { runIOAPI :: ReaderT ApiSettings IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadReader ApiSettings)

instance GraphQLApi IOApi where
  introspect = do
    settings <- asks clientSettings
    liftIO $ runGraphQLClientIO settings runIntrospection
