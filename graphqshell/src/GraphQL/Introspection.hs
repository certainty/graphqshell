{-# LANGUAGE QuasiQuotes, DuplicateRecordFields, DeriveAnyClass #-}

module GraphQL.Introspection (introspect) where
import Relude hiding (ByteString)
import Data.ByteString.Lazy
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)
import Text.RawString.QQ
import GraphQL.Types
  (
    GraphQLResponse(..)
  )
import Control.Exception.Safe (MonadThrow, throwM)

data IntrospectionError = IntrospectionError String deriving (Eq, Show)

instance Exception IntrospectionError


introspect :: (MonadIO m, MonadThrow m) => (Text -> m ByteString) -> m Schema
introspect runQuery = do
  response <- parseIntrospection <$> runQuery introspectionQuery
  case response of
    Left e  -> throwM (IntrospectionError e)
    Right resp -> case graphQLResponseData resp of
                    Just re -> pure (Schema (schema re))
                    Nothing    -> throwM (IntrospectionError "Error response from server")

parseIntrospection ::  ByteString -> Either String (GraphQLResponse IntrospectionResponse)
parseIntrospection jsonResponse = eitherDecode jsonResponse

-- | The schema that holds provides convenience access to schema related functionality
data Schema = Schema {
  internal :: IntrospectionSchema
} deriving (Eq, Show)

data IntrospectionResponse = IntrospectionResponse {
  schema :: IntrospectionSchema 
} deriving (Show, Eq, Generic, FromJSON)

data IntrospectionSchema = IntrospectionSchema {
    queryType :: RootTypeName
  , mutationType :: Maybe RootTypeName
  , subscriptionType :: Maybe RootTypeName
  , types :: [OutputType]
  , directives :: [Directive]
}  deriving (Show, Eq, Generic, FromJSON)

data RootTypeName = RootTypeName { name :: Text } deriving (Show, Eq, Generic, FromJSON)

data OutputType = OutputType {
      kind :: Text
    , name :: Text
    , description :: Maybe Text
    , fields :: Maybe [Field]
    , inputFields :: Maybe [TypeRef]
    , interfaces ::  Maybe [TypeRef]
    , enumValues :: Maybe [EnumValues]
    , possibleTypes :: Maybe [TypeRef]
} deriving (Show, Eq, Generic, FromJSON)


data EnumValues = EnumValues {
    name :: Text
  , description :: Maybe Text
  , isDeprecated :: Bool
} deriving (Show, Eq, Generic, FromJSON)

data Field = Field {
      name :: Text
    , description :: Maybe Text
    , args :: [InputType]
    , isDeprecated :: Bool
    , deprecationReason :: Maybe Text
    , typeRef :: TypeRef
} deriving (Show, Eq, Generic, FromJSON)

data TypeRef = TypeRef {
    kind :: Text
  , name :: Maybe Text
  , ofType :: TypeRef
} deriving (Show, Eq, Generic, FromJSON)

data InputType = InputType {
   name :: Text
  , description :: Maybe Text
  , typeRef :: TypeRef
  , defaultValue :: Maybe Text
} deriving (Show, Eq, Generic, FromJSON)

data Directive = Directive {
    name :: Text
  , description :: Maybe Text
  , locations :: [Text]
  , args :: Maybe [InputType]
} deriving (Show, Eq, Generic, FromJSON)
  
introspectionQuery :: Text
introspectionQuery = [r|
query Introspection {
  schema: __schema {
    queryType {
      name 
    }
    mutationType {
      name
    }
    subscriptionType {
      name
    }
    types {
      ...FullType
    }
    directives {
      name
      description
      locations
      args {
        ...InputValue
      }
    }
  }
}

fragment FullType on __Type {
  kind
  name
  description
  fields(includeDeprecated: true) {
    name
    description
    args {
      ...InputValue
    }
    typeRef: type {
      ...TypeRef
    }
    isDeprecated
    deprecationReason
  }
  inputFields {
    ...InputValue
  }
  interfaces {
    ...TypeRef
  }
  enumValues(includeDeprecated: true) {
    name
    description
    isDeprecated
    deprecationReason
  }
  possibleTypes {
    ...TypeRef
  }
}
fragment InputValue on __InputValue {
  name
  description
  typeRef: type {
    ...TypeRef
  }
  defaultValue
}
fragment TypeRef on __Type {
  kind
  name
  ofType {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    }
  }
}|]


