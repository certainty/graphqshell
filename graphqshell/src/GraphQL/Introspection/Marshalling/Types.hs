{-# LANGUAGE QuasiQuotes, DuplicateRecordFields, DeriveAnyClass #-}

module GraphQL.Introspection.Marshalling.Types where
import Relude hiding (ByteString, Type)
import Data.Aeson as J
import Text.RawString.QQ
import GraphQL.Marshalling.Utils (aesonOptions)

data IntrospectionResponse = IntrospectionResponse {
  introspectionResponsSchema :: IntrospectionSchema 
} deriving (Show, Eq, Generic)

instance FromJSON IntrospectionResponse where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionResponse")

data IntrospectionSchema = IntrospectionSchema {
    introspectionSchemaQueryType :: RootTypeName
  , introspectionSchemaMutationType :: Maybe RootTypeName
  , introspectionSchemaSubscriptionType :: Maybe RootTypeName
  , introspectionSchemaTypes :: [Type]
  , introspectionSchemaDirectives :: [Directive]
}  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionSchema where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionSchema")

data RootTypeName = RootTypeName {
  introspectionRootTypeName :: Text
} deriving (Show, Eq, Generic)

instance FromJSON RootTypeName where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionRootType")

data Type = Type {
      typeKind :: Text
    , typeName :: Text
    , typeDescription :: Maybe Text
    , typeFields :: Maybe [Field]
    , typeInputFields :: Maybe [InputType]
    , typeInterfaces ::  Maybe [TypeRef]
    , typeEnumValues :: Maybe [EnumValue]
    , typePossibleTypes :: Maybe [TypeRef]
} deriving (Show, Eq, Generic)

instance FromJSON Type where
  parseJSON = J.genericParseJSON (aesonOptions "type")

data EnumValue = EnumValue {
    enumValueName :: Text
  , enumValueDescription :: Maybe Text
  , enumValueIsDeprecated :: Bool
  , enumValueDeprecationReason :: Maybe Text
} deriving (Show, Eq, Generic)


instance FromJSON EnumValue where
  parseJSON = J.genericParseJSON (aesonOptions "enumValue")

data Field = Field {
      fieldName :: Text
    , fieldDescription :: Maybe Text
    , fieldArgs :: [InputType]
    , fieldIsDeprecated :: Bool
    , fieldDeprecationReason :: Maybe Text
    , fieldTypeRef :: TypeRef
} deriving (Show, Eq, Generic)

instance FromJSON Field where
  parseJSON = J.genericParseJSON (aesonOptions "field")

data TypeRef = TypeRef {
    typeRefKind :: Text
  , typeRefName :: Maybe Text
  , typeRefOfType :: Maybe TypeRef
} deriving (Show, Eq, Generic)

instance FromJSON TypeRef where
  parseJSON = J.genericParseJSON (aesonOptions "typeRef")

data InputType = InputType {
    inputTypeName :: Text
  , inputTypeDescription :: Maybe Text
  , inputTypeTypeRef :: TypeRef
  , inputTypeDefaultValue :: Maybe Text
} deriving (Show, Eq, Generic)

instance FromJSON InputType where
  parseJSON = J.genericParseJSON (aesonOptions "inputType")

data Directive = Directive {
    directiveName :: Text
  , directiveDescription :: Maybe Text
  , directiveLocations :: [Text]
  , directiveArgs :: Maybe [InputType]
} deriving (Show, Eq, Generic)


instance FromJSON Directive where
  parseJSON = J.genericParseJSON (aesonOptions "directive")

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
