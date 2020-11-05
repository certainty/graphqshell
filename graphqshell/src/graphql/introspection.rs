use crate::graphql::client;
use serde::Deserialize;
use serde_json::Value;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Client connection error")]
    ClientError(#[from] client::Error),

    #[error("Introspection query returned an error")]
    IntrospectionQueryFailed(String),
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::IntrospectionQueryFailed(format!(
            "Failed to deserialize introspection response: {}",
            e
        ))
    }
}

// Api response types
#[derive(Debug, Clone, Deserialize)]
struct IntrospectionResponse {
    #[serde(rename = "__schema")]
    schema: IntrospectionSchema,
}

#[derive(Debug, Clone, Deserialize)]
struct Schema {
    #[serde(rename = "queryType")]
    query: IntroSpectionRootType,

    #[serde(rename = "mutationType")]
    mutation: Option<IntroSpectionRootType>,

    #[serde(rename = "subscriptionType")]
    subscription: Option<IntroSpectionRootType>,

    types: Vec<IntrospectionOutputType>,

    directives: Vec<IntroSpectionDirective>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionOutputType {
    kind: String,
    name: String,
    description: Option<String>,
    fields: Option<Vec<IntrospectionField>>,

    #[serde(rename = "inputFields")]
    input_fields: Option<Vec<IntrospectionInputType>>,

    interfaces: Option<Vec<IntrospectionInputRef>>,

    #[serde(rename = "enumValues")]
    enum_values: Option<Vec<IntrospectionEnumValues>>,

    #[serde(rename = "possibleTypes")]
    possible_types: Option<Vec<IntrospectionInputRef>>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionEnumValues {
    name: String,
    description: Option<String>,

    #[serde(rename = "isDeprecated")]
    is_deprecated: bool,

    #[serde(rename = "deprecationReason")]
    deprecation_reason: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionField {
    name: String,
    description: Option<String>,

    #[serde(rename = "args")]
    arguments: Vec<IntrospectionInputType>,

    #[serde(rename = "isDeprecated")]
    is_deprecated: bool,

    #[serde(rename = "deprecationReason")]
    deprecation_reason: Option<String>,

    #[serde(rename = "type")]
    type_ref: IntrospectionInputRef,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputType {
    name: String,
    description: Option<String>,

    #[serde(rename = "type")]
    type_ref: IntrospectionInputRef,

    #[serde(rename = "defaultValue")]
    default_value: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputRef {
    kind: String,
    name: Option<String>,
    #[serde(rename = "ofType")]
    of_type: Option<IntrospectionInputRef2>,
}

// Hack around recursive structs
#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputRef2 {
    kind: String,
    name: Option<String>,
    #[serde(rename = "ofType")]
    of_type: Option<IntrospectionInputRef3>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputRef3 {
    kind: String,
    name: Option<String>,
    #[serde(rename = "ofType")]
    of_type: Option<IntrospectionInputRef4>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputRef4 {
    kind: String,
    name: Option<String>,
    #[serde(rename = "ofType")]
    of_type: Option<IntrospectionInputRef5>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputRef5 {
    kind: String,
    name: Option<String>,
    #[serde(rename = "ofType")]
    of_type: Option<IntrospectionInputRef6>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputRef6 {
    kind: String,
    name: Option<String>,
    of_type: Option<IntrospectionInputRef7>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputRef7 {
    kind: String,
    name: Option<String>,
    #[serde(rename = "ofType")]
    of_type: Option<IntrospectionInputRefBase>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntrospectionInputRefBase {
    kind: String,
    name: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct IntroSpectionRootType {
    name: String,
}

#[derive(Debug, Clone, Deserialize)]
struct IntroSpectionDirective {
    name: String,
    description: Option<String>,
    locations: Vec<String>,
    #[serde(rename = "args")]
    arguments: Option<Vec<IntrospectionInputType>>,
}

pub fn introspect(client: &client::Client) -> Result<Schema, Error> {
    let response = client.request(INTROSPECTION_QUERY.into())?;

    if response.is_partial() {
        Err(Error::IntrospectionQueryFailed(
            "Partial result received from API on introspection query".into(),
        ))
    } else {
        let introspection_response: IntrospectionResponse =
            serde_json::from_value(response.data.unwrap())?;

        println!("{:?}", introspection_response);

        Ok(introspection_response.schema)
    }
}

const INTROSPECTION_QUERY: &str = "
query IntrospectionQuery {
  __schema {
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
    type {
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
  type {
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
}
";
