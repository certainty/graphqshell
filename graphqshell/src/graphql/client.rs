use serde;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::time::Duration;
use thiserror::Error;
use url::Url;

#[derive(Debug, Clone)]
pub struct ClientSettings {
    url: Url,
    timeout: Duration,
    gzip: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    #[serde(rename = "operationName")]
    operation_name: Option<String>,
    query: String,
    variables: Option<Vec<(String, Value)>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub data: Option<Value>,
    pub errors: Option<Vec<Value>>,
}

impl Response {
    pub fn is_partial(&self) -> bool {
        self.errors.is_some()
    }
}

impl ClientSettings {
    pub fn default(url: Url) -> ClientSettings {
        ClientSettings {
            url: url,
            gzip: false,
            timeout: Duration::from_secs(10),
        }
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("HTTP request failed")]
    HttpError(#[from] reqwest::Error),
}

#[derive(Debug)]
pub struct Client {
    settings: ClientSettings,
    client: reqwest::blocking::Client,
}

impl Client {
    pub fn new(settings: ClientSettings) -> Result<Client, Error> {
        let client = reqwest::blocking::Client::new();

        Ok(Client {
            client: client,
            settings: settings,
        })
    }

    pub fn request(&self, query: String) -> Result<Response, Error> {
        let req = Request {
            query: query,
            operation_name: None,
            variables: None,
        };

        let res: Response = self
            .client
            .post(self.settings.url.to_owned())
            .json(&req)
            .send()?
            .json()?;

        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;
    extern crate mockito;

    #[test]
    fn test_success() {
        let mock = mockito::mock("POST", "/graphql/api")
            .with_status(200)
            .with_body("{ \"data\": { }  }")
            .create();

        let url = Url::parse(mockito::server_url().as_str())
            .unwrap()
            .join("graphql/api")
            .unwrap();

        let client = Client::new(ClientSettings::default(url)).unwrap();
        let response = client
            .request("query foo { humans { id } }".into())
            .unwrap();

        assert!(!response.is_partial());
        mock.assert();
    }

    #[test]
    fn test_errors() {
        let mock = mockito::mock("POST", "/graphql/api")
            .with_status(200)
            .with_body("{ \"errors\": [{}]  }")
            .create();

        let url = Url::parse(mockito::server_url().as_str())
            .unwrap()
            .join("graphql/api")
            .unwrap();

        let client = Client::new(ClientSettings::default(url)).unwrap();
        let response = client
            .request("query foo { humans { id } }".into())
            .unwrap();

        assert!(response.is_partial());
        mock.assert();
    }
}
