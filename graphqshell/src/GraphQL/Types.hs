module GraphQL.Types(
   GraphQLQuery(..)
  , Location(..)
  , GraphQLError(..)
  , GraphQLQueryError(..)
  , GraphQLResponse(..)
  , GraphQLBody(..)
  , Nodes(..)
 ) where

import Relude
import qualified Data.Aeson                    as J
import           Data.Char                      ( toLower )

-- map record field names to correct json field names by removing a common prefix
defaultJSONOptions :: String -> J.Options
defaultJSONOptions n = J.defaultOptions
  {
    J.fieldLabelModifier = lowerFirst . drop (length n)
  }
 where
  lowerFirst []       = []
  lowerFirst (x : xs) = (toLower x) : xs

-----------------------------------------------------------------------------
newtype GraphQLQuery = GraphQLQuery { unGraphQLQuery :: Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON, IsString, Generic)

-----------------------------------------------------------------------------
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

instance J.ToJSON GraphQLError where
  toJSON = J.genericToJSON $ defaultJSONOptions "GraphQLError"
  
instance J.FromJSON GraphQLError where
  parseJSON = J.genericParseJSON $ defaultJSONOptions "GraphQLError"

data Location = Location
  { locationLine   :: Integer
  , locationColumn :: Integer
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON Location where
  toJSON = J.genericToJSON $ defaultJSONOptions "Location"
  
instance J.FromJSON Location where
  parseJSON = J.genericParseJSON $ defaultJSONOptions "Location"


-- | The response to the GraphQL query.
data GraphQLResponse a = GraphQLResponse
  { graphQLResponseData   :: Maybe a
  , graphQLResponseErrors :: Maybe [GraphQLError]
  }
  deriving (Eq, Show, Generic)

instance J.ToJSON a => J.ToJSON (GraphQLResponse a)  where
  toJSON = J.genericToJSON $ defaultJSONOptions "GraphQLResponse"
  
instance J.FromJSON a => J.FromJSON (GraphQLResponse a) where
  parseJSON = J.genericParseJSON $ defaultJSONOptions "GraphQLResponse"

data Nodes a = Nodes
  { nodes :: [a]
  }
  deriving (Eq, Show, Generic)

instance (J.ToJSON a) => J.ToJSON (Nodes a) where
  toJSON = J.genericToJSON J.defaultOptions
instance (J.FromJSON a) => J.FromJSON (Nodes a)

-----------------------------------------------------------------------------
-- | body object expected by GraphQL APIs.
data GraphQLBody a = GraphQLBody
  { graphQLBodyQuery     :: GraphQLQuery
  , graphQLBodyVariables :: Maybe a
  -- add operations
  }
  deriving (Eq, Show, Generic)

instance (J.ToJSON a) => J.ToJSON (GraphQLBody a) where
  toJSON = J.genericToJSON $ defaultJSONOptions "GraphQLBody"
  
instance (J.FromJSON a) => J.FromJSON (GraphQLBody a) where
  parseJSON = J.genericParseJSON $ defaultJSONOptions "GraphQLBody"

