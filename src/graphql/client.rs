use url::Url;
use std::time::Duration;

pub struct ClientSettings {
    base_url: Url,
    timeout: Duration,
    gzip: bool
}

impl ClientSettings {
    pub fn default(base_url: Url) -> ClientSettings {
        ClientSettings {
            base_url: base_url,
            gzip: false,
            timeout: Duration::from_secs(10),
        }
    }
}

pub struct Client {
    settings: ClientSettings,
    client: reqwest::Client,
}

impl Client {
    pub fn new(client_settings: ClientSettings) -> Result<Client, ()> {
       let client = reqwest::Client::new();

       Ok(Client {
           client: client,
           settings: client_settings,
       })
    }    
}
