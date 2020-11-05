use crate::graphql::client::Client;
use crate::graphql::schema::introspection;

#[derive(Debug)]
pub struct Api {
    client: Client,
}

impl Api {
    pub fn new(client: Client) -> Self {
        Self { client: client }
    }

    pub fn schema(&self) -> Result<introspection::Schema, introspection::Error> {
        introspection::introspect(&self.client)
    }
}
