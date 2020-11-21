{-# LANGUAGE MultiParamTypeClasses #-}

module GraphQL.Client.Types(
    GraphQLQuery(..)
  , Location(..)
  , GraphQLError(..)
  , GraphQLQueryError(..)
  , GraphQLResponse(..)
  , GraphQLBody(..)
  , GraphQLClient(..)
  , emptyVariables
 ) where

import Relude
import qualified Data.Aeson as J
import GraphQL.Marshalling.Utils (aesonOptions)
import Control.Exception.Safe (MonadThrow)
import Data.Aeson (FromJSON, ToJSON)

class (Monad m, MonadThrow m) => GraphQLClient m where
  runGraphQLRequest :: (ToJSON variables, FromJSON resp) => GraphQLQuery -> Maybe variables ->  m (GraphQLResponse resp)

-- Request portion
newtype GraphQLQuery = GraphQLQuery { unGraphQLQuery :: Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON, IsString, Generic)

data GraphQLBody a = GraphQLBody
  { graphQLBodyQuery     :: GraphQLQuery
  , graphQLBodyVariables :: Maybe a
  -- add operations
  }
  deriving (Eq, Show, Generic)

instance (J.ToJSON a) => J.ToJSON (GraphQLBody a) where
  toJSON = J.genericToJSON $ aesonOptions "GraphQLBody"
  
instance (J.FromJSON a) => J.FromJSON (GraphQLBody a) where
  parseJSON = J.genericParseJSON $ aesonOptions "GraphQLBody"

-- Response portion
data GraphQLQueryError = EmptyGraphQLReponse
                       | HttpError Text
                       | ParsingError Text
  deriving (Show, Eq, Generic)

instance Exception GraphQLQueryError

data GraphQLError = GraphQLError
  { graphQLErrorMessage   :: Text
  , graphQLErrorLocations :: Maybe [Location]
  , graphQLErrorPath      :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON GraphQLError where
  parseJSON = J.genericParseJSON $ aesonOptions "GraphQLError"

data Location = Location
  { locationLine   :: Integer
  , locationColumn :: Integer
  }
  deriving (Show, Eq, Generic)

instance J.FromJSON Location where
  parseJSON = J.genericParseJSON $ aesonOptions "Location"

-- TODO: Distringuish Success, Partial, Error
data GraphQLResponse a = GraphQLResponse
  { graphQLResponseData   :: Maybe a
  , graphQLResponseErrors :: Maybe [GraphQLError]
  }
  deriving (Eq, Show, Generic)

instance J.FromJSON a => J.FromJSON (GraphQLResponse a) where
  parseJSON = J.genericParseJSON $ aesonOptions "GraphQLResponse"

emptyVariables :: Maybe ()
emptyVariables = Nothing 
