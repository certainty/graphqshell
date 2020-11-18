{-|
This module provides types to represent introspection data
as sent by a GraphQL server implementation.

You should not need use it directly but instead interact with
the schema with the higher level 'GraphQL.Introspection.Schema' API.
-}

{-# LANGUAGE QuasiQuotes #-}

module GraphQL.Introspection.Marshalling.Types where
import Relude hiding (ByteString, Type)
import Data.Aeson as J
import Text.RawString.QQ
import GraphQL.Marshalling.Utils (aesonOptions)
import Data.Vector (Vector)

data IntrospectionResponse = IntrospectionResponse {
  introspectionResponsSchema :: IntrospectionSchema 
} deriving (Show, Eq, Generic)

instance FromJSON IntrospectionResponse where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionResponse")

data IntrospectionSchema = IntrospectionSchema {
    introspectionSchemaQueryType :: IntrospectionRootTypeName
  , introspectionSchemaMutationType :: Maybe IntrospectionRootTypeName
  , introspectionSchemaSubscriptionType :: Maybe IntrospectionRootTypeName
  , introspectionSchemaTypes :: Vector IntrospectionType
  , introspectionSchemaDirectives :: Vector IntrospectionDirective
}  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionSchema where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionSchema")

data IntrospectionRootTypeName = RootTypeName {
  introspectionRootTypeName :: Text
} deriving (Show, Eq, Generic)

instance FromJSON IntrospectionRootTypeName where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionRootType")

data IntrospectionType = IntrospectionType {
      introspectionTypeKind :: Text
    , introspectionTypeName :: Text
    , introspectionTypeDescription :: Maybe Text
    , introspectionTypeFields :: Maybe (Vector IntrospectionField)
    , introspectionTypeInputFields :: Maybe (Vector IntrospectionInputType)
    , introspectionTypeInterfaces ::  Maybe (Vector IntrospectionTypeRef)
    , introspectionTypeEnumValues :: Maybe (Vector IntrospectionEnumValue)
    , introspectionTypePossibleTypes :: Maybe (Vector IntrospectionTypeRef)
} deriving (Show, Eq, Generic)

instance FromJSON IntrospectionType where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionType")

data IntrospectionEnumValue = IntrospectionEnumValue {
    introspectionEnumValueName :: Text
  , introspectionEnumValueDescription :: Maybe Text
  , introspectionEnumValueIsDeprecated :: Bool
  , introspectionEnumValueDeprecationReason :: Maybe Text
} deriving (Show, Eq, Generic)


instance FromJSON IntrospectionEnumValue where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionEnumValue")

data IntrospectionField = IntrospectionField {
      introspectionFieldName :: Text
    , introspectionFieldDescription :: Maybe Text
    , introspectionFieldArgs :: Vector IntrospectionInputType
    , introspectionFieldIsDeprecated :: Bool
    , introspectionFieldDeprecationReason :: Maybe Text
    , introspectionFieldTypeRef :: IntrospectionTypeRef
} deriving (Show, Eq, Generic)

instance FromJSON IntrospectionField where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionField")

data IntrospectionTypeRef = IntrospectionTypeRef {
    introspectionTypeRefKind :: Text
  , introspectionTypeRefName :: Maybe Text
  , introspectionTypeRefOfType :: Maybe IntrospectionTypeRef
} deriving (Show, Eq, Generic)

instance FromJSON IntrospectionTypeRef where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionTypeRef")

data IntrospectionInputType = IntrospectionInputType {
    introspectionInputTypeName :: Text
  , introspectionInputTypeDescription :: Maybe Text
  , introspectionInputTypeTypeRef :: IntrospectionTypeRef
  , introspectionInputTypeDefaultValue :: Maybe Text
} deriving (Show, Eq, Generic)

instance FromJSON IntrospectionInputType where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionInputType")

data IntrospectionDirective = IntrospectionDirective {
    introspectionDirectiveName :: Text
  , introspectionDirectiveDescription :: Maybe Text
  , introspectionDirectiveLocations :: Vector Text
  , introspectionDirectiveArgs :: Maybe (Vector IntrospectionInputType)
} deriving (Show, Eq, Generic)

instance FromJSON IntrospectionDirective where
  parseJSON = J.genericParseJSON (aesonOptions "introspectionDirective")

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
