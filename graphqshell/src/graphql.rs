pub mod api;
pub mod client;
pub mod schema;

use api::Api;
use client::{Client, ClientSettings};
use thiserror::Error;
use url::Url;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Client connection failed")]
    ClientError(#[from] client::Error),
}

/// Connect to a remote GraphQL API
///
/// When successful this function returns an Api object which is
/// the primary interface to the remote GraphQL Api.
/// It provides all the required functionality for introspection and
/// querying.
///
/// If you really need to you can drop down to the low level `Client` interface
/// by accessing the `Api.client` field but in most cases that should not be required.
pub fn api(url: Url) -> Result<Api, Error> {
    let client = Client::new(ClientSettings::default(url))?;
    Ok(Api::new(client))
}
