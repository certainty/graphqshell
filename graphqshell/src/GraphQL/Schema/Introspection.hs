{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}

module GraphQL.Schema.Introspection (schemaFromIntrospectionResponse, introspectionQuery, Schema) where
import Relude
import Data.Either.Combinators (mapLeft)
import Text.RawString.QQ
import Data.Aeson (eitherDecode)
import GraphQL.Schema.Introspection.Internal
data IntrospectionError = IntrospectionError String deriving (Eq, Show, Exception)



newtype Schema = Schema {
  internal :: IntrospectionSchema
} deriving (Eq, Show)


schemaFromIntrospectionResponse :: LByteString -> Either IntrospectionError Schema
schemaFromIntrospectionResponse jsonResponse = Schema <$> parseResponse 
 where
   parseResponse :: Either IntrospectionError IntrospectionSchema
   parseResponse = schema <$> mapLeft IntrospectionError (eitherDecode jsonResponse)

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


