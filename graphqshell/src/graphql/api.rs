use crate::graphql::client::Client;
use crate::graphql::schema::introspection;
use crate::graphql::schema::Schema;

#[derive(Debug, Clone)]
pub struct Api {
    client: Client,
}

impl Api {
    pub fn new(client: Client) -> Self {
        Self { client: client }
    }

    pub fn schema(&self) -> Result<Schema, introspection::Error> {
        let schema = introspection::introspect(&self.client)?;

        Ok(Schema::from(schema))
    }
}
