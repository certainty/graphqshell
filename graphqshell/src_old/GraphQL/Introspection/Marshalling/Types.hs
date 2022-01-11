{-# LANGUAGE QuasiQuotes #-}

-- |
-- This module provides types to represent introspection data
-- as sent by a GraphQL server implementation.
--
-- You should not need to use it directly but instead interact with
-- the schema with the higher level 'GraphQL.Introspection' and 'GraphQL.Introspection.Schema' API.
module GraphQL.Introspection.Marshalling.Types where

import Data.Aeson (FromJSON, genericParseJSON, parseJSON)
import qualified Data.Aeson.Types as J
import Data.Vector (Vector)
import GraphQL.Marshalling.Utils (aesonOptions)
import Relude
  ( Generic,
    Text,
  )
import Text.RawString.QQ (r)
import Prelude

newtype IntrospectionResponse = IntrospectionResponse
  { introspectionResponseSchema :: IntrospectionSchema
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionResponse where
  parseJSON = genericParseJSON (aesonOptions "introspectionResponse")

data IntrospectionSchema = IntrospectionSchema
  { introspectionSchemaQueryType :: IntrospectionType,
    introspectionSchemaMutationType :: Maybe IntrospectionType,
    introspectionSchemaSubscriptionType :: Maybe IntrospectionType,
    introspectionSchemaTypes :: Vector IntrospectionType,
    introspectionSchemaDirectives :: Vector IntrospectionDirective
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionSchema where
  parseJSON = genericParseJSON (aesonOptions "introspectionSchema")

newtype IntrospectionRootTypeName = RootTypeName
  { introspectionRootTypeName :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionRootTypeName where
  parseJSON = genericParseJSON (aesonOptions "introspectionRootType")

data IntrospectionTypeKind = Scalar | Enum | Object | Interface | Union | InputObject | List | NonNull deriving (Eq, Show)

data IntrospectionType = IntrospectionType
  { introspectionTypeKind :: IntrospectionTypeKind,
    introspectionTypeName :: Text,
    introspectionTypeDescription :: Maybe Text,
    introspectionTypeFields :: Maybe (Vector IntrospectionField),
    introspectionTypeInputFields :: Maybe (Vector IntrospectionInputType),
    introspectionTypeInterfaces :: Maybe (Vector IntrospectionTypeRef),
    introspectionTypeEnumValues :: Maybe (Vector IntrospectionEnumValue),
    introspectionTypePossibleTypes :: Maybe (Vector IntrospectionTypeRef)
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionTypeKind where
  parseJSON (J.String "SCALAR") = pure Scalar
  parseJSON (J.String "OBJECT") = pure Object
  parseJSON (J.String "INTERFACE") = pure Interface
  parseJSON (J.String "UNION") = pure Union
  parseJSON (J.String "INPUT_OBJECT") = pure InputObject
  parseJSON (J.String "LIST") = pure List
  parseJSON (J.String "NON_NULL") = pure NonNull
  parseJSON (J.String "ENUM") = pure Enum
  parseJSON invalidKind = fail $ "Invalid kind encountered: " <> show invalidKind

instance FromJSON IntrospectionType where
  parseJSON = genericParseJSON (aesonOptions "introspectionType")

data IntrospectionEnumValue = IntrospectionEnumValue
  { introspectionEnumValueName :: Text,
    introspectionEnumValueDescription :: Maybe Text,
    introspectionEnumValueIsDeprecated :: Bool,
    introspectionEnumValueDeprecationReason :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionEnumValue where
  parseJSON = genericParseJSON (aesonOptions "introspectionEnumValue")

data IntrospectionField = IntrospectionField
  { introspectionFieldName :: Text,
    introspectionFieldDescription :: Maybe Text,
    introspectionFieldArgs :: Vector IntrospectionInputType,
    introspectionFieldIsDeprecated :: Bool,
    introspectionFieldDeprecationReason :: Maybe Text,
    introspectionFieldTypeRef :: IntrospectionTypeRef
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionField where
  parseJSON = genericParseJSON (aesonOptions "introspectionField")

data IntrospectionTypeRef = IntrospectionTypeRef
  { introspectionTypeRefKind :: IntrospectionTypeKind,
    introspectionTypeRefName :: Maybe Text,
    introspectionTypeRefOfType :: Maybe IntrospectionTypeRef
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionTypeRef where
  parseJSON = genericParseJSON (aesonOptions "introspectionTypeRef")

data IntrospectionInputType = IntrospectionInputType
  { introspectionInputTypeName :: Text,
    introspectionInputTypeDescription :: Maybe Text,
    introspectionInputTypeTypeRef :: IntrospectionTypeRef,
    introspectionInputTypeDefaultValue :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionInputType where
  parseJSON = genericParseJSON (aesonOptions "introspectionInputType")

data IntrospectionDirective = IntrospectionDirective
  { introspectionDirectiveName :: Text,
    introspectionDirectiveDescription :: Maybe Text,
    introspectionDirectiveLocations :: Vector Text,
    introspectionDirectiveArgs :: Maybe (Vector IntrospectionInputType)
  }
  deriving (Show, Eq, Generic)

instance FromJSON IntrospectionDirective where
  parseJSON = genericParseJSON (aesonOptions "introspectionDirective")

-- This is not a type but it is closely related to the types defined here.
-- It determines how they need to be marshalled
introspectionQuery :: Text
introspectionQuery =
  [r|
query Introspection {
  schema: __schema {
    queryType {
      ...FullType
    }
    mutationType {
      ...FullType
    }
    subscriptionType {
      ...FullType
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
