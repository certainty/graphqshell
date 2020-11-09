{-# LANGUAGE QuasiQuotes, DuplicateRecordFields, DeriveAnyClass #-}

module GraphQL.Schema.Introspection.Internal where
import Relude hiding (ByteString)
import Data.Aeson.Types (FromJSON)


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
    , inputFields :: Maybe [InputType]
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
  , ofType :: Maybe TypeRef
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


