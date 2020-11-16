{-# LANGUAGE QuasiQuotes, DuplicateRecordFields, DeriveAnyClass #-}

module GraphQL.Introspection.Marshalling.Types where
import Relude hiding (ByteString, Type)
import Data.Aeson as J
import Text.RawString.QQ
import GraphQL.Marshalling.Utils (aesonOptions)

data IntrospectionResponse = IntrospectionResponse {
  schema :: IntrospectionSchema 
} deriving (Show, Eq, Generic, FromJSON)

data IntrospectionSchema = IntrospectionSchema {
    queryType :: RootTypeName
  , mutationType :: Maybe RootTypeName
  , subscriptionType :: Maybe RootTypeName
  , types :: [Type]
  , directives :: [Directive]
}  deriving (Show, Eq, Generic, FromJSON)

data RootTypeName = RootTypeName { irName :: Text } deriving (Show, Eq, Generic)

instance FromJSON RootTypeName where
  parseJSON = J.genericParseJSON (aesonOptions "ir")

data Type = Type {
      itpeKind :: Text
    , itpeName :: Text
    , itpeDescription :: Maybe Text
    , itpeFields :: Maybe [Field]
    , itpeInputFields :: Maybe [InputType]
    , itpeInterfaces ::  Maybe [TypeRef]
    , itpeEnumValues :: Maybe [EnumValues]
    , itpePossibleTypes :: Maybe [TypeRef]
} deriving (Show, Eq, Generic)

instance FromJSON Type where
  parseJSON = J.genericParseJSON (aesonOptions "itpe")

data EnumValues = EnumValues {
    ievName :: Text
  , ievDescription :: Maybe Text
  , ievIsDeprecated :: Bool
  , ievDeprecationReason :: Maybe Text
} deriving (Show, Eq, Generic)


instance FromJSON EnumValues where
  parseJSON = J.genericParseJSON (aesonOptions "iev")

data Field = Field {
      ifName :: Text
    , ifDescription :: Maybe Text
    , ifArgs :: [InputType]
    , ifIsDeprecated :: Bool
    , ifDeprecationReason :: Maybe Text
    , ifTypeRef :: TypeRef
} deriving (Show, Eq, Generic)

instance FromJSON Field where
  parseJSON = J.genericParseJSON (aesonOptions "if")

data TypeRef = TypeRef {
    itrKind :: Text
  , itrName :: Maybe Text
  , itrOfType :: Maybe TypeRef
} deriving (Show, Eq, Generic)

instance FromJSON TypeRef where
  parseJSON = J.genericParseJSON (aesonOptions "itr")

data InputType = InputType {
    iiName :: Text
  , iiDescription :: Maybe Text
  , iiTypeRef :: TypeRef
  , iiDefaultValue :: Maybe Text
} deriving (Show, Eq, Generic)

instance FromJSON InputType where
  parseJSON = J.genericParseJSON (aesonOptions "ii")

data Directive = Directive {
    idName :: Text
  , idDescription :: Maybe Text
  , idLocations :: [Text]
  , idArgs :: Maybe [InputType]
} deriving (Show, Eq, Generic)


instance FromJSON Directive where
  parseJSON = J.genericParseJSON (aesonOptions "id")

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
