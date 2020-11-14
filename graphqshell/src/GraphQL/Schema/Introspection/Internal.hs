{-# LANGUAGE QuasiQuotes, DuplicateRecordFields, DeriveAnyClass #-}

module GraphQL.Schema.Introspection.Internal where
import Relude hiding (ByteString, Type)
import Data.Aeson as J
import Text.RawString.QQ
import Data.Char (toLower)

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

data RootTypeName = RootTypeName { irName :: String } deriving (Show, Eq, Generic)

instance FromJSON RootTypeName where
  parseJSON = J.genericParseJSON (aesonOptions "ir")

data Type = Type {
      itpeKind :: String
    , itpeName :: String
    , itpeDescription :: Maybe String
    , itpeFields :: Maybe [Field]
    , itpeInputFields :: Maybe [InputType]
    , itpeInterfaces ::  Maybe [TypeRef]
    , itpeEnumValues :: Maybe [EnumValues]
    , itpePossibleTypes :: Maybe [TypeRef]
} deriving (Show, Eq, Generic)

instance FromJSON Type where
  parseJSON = J.genericParseJSON (aesonOptions "itpe")

data EnumValues = EnumValues {
    ievName :: String
  , ievDescription :: Maybe String
  , ievIsDeprecated :: Bool
  , ievDeprecationReason :: Maybe String
} deriving (Show, Eq, Generic)


instance FromJSON EnumValues where
  parseJSON = J.genericParseJSON (aesonOptions "iev")

data Field = Field {
      ifName :: String
    , ifDescription :: Maybe String
    , ifArgs :: [InputType]
    , ifIsDeprecated :: Bool
    , ifDeprecationReason :: Maybe String
    , ifTypeRef :: TypeRef
} deriving (Show, Eq, Generic)

instance FromJSON Field where
  parseJSON = J.genericParseJSON (aesonOptions "if")

data TypeRef = TypeRef {
    itrKind :: String
  , itrName :: Maybe String
  , itrOfType :: Maybe TypeRef
} deriving (Show, Eq, Generic)

instance FromJSON TypeRef where
  parseJSON = J.genericParseJSON (aesonOptions "itr")

data InputType = InputType {
    iiName :: String
  , iiDescription :: Maybe String
  , iiTypeRef :: TypeRef
  , iiDefaultValue :: Maybe String
} deriving (Show, Eq, Generic)

instance FromJSON InputType where
  parseJSON = J.genericParseJSON (aesonOptions "ii")

data Directive = Directive {
    idName :: String
  , idDescription :: Maybe String
  , idLocations :: [String]
  , idArgs :: Maybe [InputType]
} deriving (Show, Eq, Generic)


instance FromJSON Directive where
  parseJSON = J.genericParseJSON (aesonOptions "id")

aesonOptions :: String -> Options
aesonOptions prefix = J.defaultOptions { J.fieldLabelModifier = (rewriteFieldName prefix)  }

rewriteFieldName :: String -> String -> String
rewriteFieldName prefix fieldName = case (drop (length prefix) fieldName) of
  (front:rear) -> (toLower front) : rear
  _            -> fieldName 

introspectionQuery :: String
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
