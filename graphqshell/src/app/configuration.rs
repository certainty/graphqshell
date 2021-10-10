use crate::utils::DTO;
use chrono::Duration;
use config;
use directories;
use serde::{Deserialize, Serialize};
use std::io::Write;
use std::path::PathBuf;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    ConfigError(#[from] config::ConfigError),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("Could not determine user directory")]
    UnknownUserDirectory,
    #[error(transparent)]
    UrlParseError(#[from] url::ParseError),
    #[error(transparent)]
    SerializationError(#[from] toml::ser::Error),
}

#[derive(Debug, Clone)]
pub struct Config {
    pub application: Application,
    pub endpoint: Vec<Endpoint>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            application: Application::default(),
            endpoint: vec![Endpoint::default()],
        }
    }
}

#[derive(Deserialize, Serialize, Clone)]
struct ConfigDTO {
    application: ApplicationDTO,
    endpoint: Vec<EndpointDTO>,
}

impl DTO<ConfigDTO, Error> for Config {
    fn from_dto(data: &ConfigDTO) -> std::result::Result<Config, Error> {
        Ok(Config {
            application: DTO::from_dto(&data.application)?,
            endpoint: data
                .endpoint
                .iter()
                .map(DTO::from_dto)
                .collect::<Result<Vec<_>>>()?,
        })
    }

    fn to_dto(&self) -> std::result::Result<ConfigDTO, Error> {
        Ok(ConfigDTO {
            application: self.application.to_dto()?,
            endpoint: self
                .endpoint
                .iter()
                .map(|e| e.to_dto())
                .collect::<Result<Vec<_>>>()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct Application {
    pub tick_rate: chrono::Duration,
}

impl Default for Application {
    fn default() -> Self {
        Self {
            tick_rate: chrono::Duration::seconds(2),
        }
    }
}

#[derive(Deserialize, Serialize, Clone)]
struct ApplicationDTO {
    tick_rate_ms: i64,
}

impl DTO<ApplicationDTO, Error> for Application {
    fn from_dto(data: &ApplicationDTO) -> std::result::Result<Self, Error> {
        // TODO: validate value of tick rate

        Ok(Self {
            tick_rate: chrono::Duration::milliseconds(data.tick_rate_ms),
        })
    }

    fn to_dto(&self) -> std::result::Result<ApplicationDTO, Error> {
        Ok(ApplicationDTO {
            tick_rate_ms: self.tick_rate.num_milliseconds(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Endpoint {
    name: String,
    url: url::Url,
    http: Option<Http>,
}

impl Default for Endpoint {
    fn default() -> Self {
        Self {
            name: String::from("default"),
            url: url::Url::parse("http://localhost:5050/graphql").unwrap(),
            http: Some(Http::default()),
        }
    }
}

#[derive(Deserialize, Serialize, Clone)]
pub struct EndpointDTO {
    name: String,
    url: String,
    http: Option<HttpDTO>,
}

impl DTO<EndpointDTO, Error> for Endpoint {
    fn from_dto(data: &EndpointDTO) -> std::result::Result<Self, Error> {
        let url = url::Url::parse(&data.url)?;
        let http = if let Some(http_cfg) = &data.http {
            Some(DTO::from_dto(http_cfg)?)
        } else {
            None
        };

        Ok(Self {
            name: data.name.clone(),
            url,
            http,
        })
    }

    fn to_dto(&self) -> std::result::Result<EndpointDTO, Error> {
        let http = if let Some(http_cfg) = &self.http {
            Some(http_cfg.to_dto()?)
        } else {
            None
        };

        Ok(EndpointDTO {
            name: self.name.clone(),
            url: self.url.to_string(),
            http,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Http {
    timeout: Option<Duration>,
    headers: Option<Vec<(String, String)>>,
}

impl Default for Http {
    fn default() -> Self {
        Self {
            timeout: Some(Duration::seconds(2)),
            headers: None,
        }
    }
}

#[derive(Deserialize, Serialize, Clone)]
pub struct HttpDTO {
    timeout_ms: Option<i64>,
    headers: Option<Vec<(String, String)>>,
}

impl DTO<HttpDTO, Error> for Http {
    fn from_dto(data: &HttpDTO) -> std::result::Result<Self, Error> {
        Ok(Self {
            timeout: data.timeout_ms.map(Duration::milliseconds),
            headers: data.headers.clone(),
        })
    }

    fn to_dto(&self) -> std::result::Result<HttpDTO, Error> {
        Ok(HttpDTO {
            timeout_ms: self.timeout.map(|d| d.num_milliseconds()),
            headers: self.headers.clone(),
        })
    }
}

pub fn load(from: std::path::PathBuf) -> Result<Config> {
    let mut cfg = config::Config::default();
    cfg.merge(config::File::from(from))?;

    let cfg_dto: ConfigDTO = cfg.try_into()?;
    let cfg = DTO::from_dto(&cfg_dto)?;

    Ok(cfg)
}

pub fn load_default() -> Result<Config> {
    load(default_directory()?)
}

pub fn init_directories() -> Result<()> {
    let base_dir = default_directory()?;

    if !base_dir.as_path().is_dir() {
        std::fs::create_dir_all(base_dir.as_path())?;

        let default_config = Config::default().to_dto()?;
        let mut file = std::fs::File::create(base_dir.join("config.toml"))?;
        file.write(toml::to_string(&default_config)?.as_bytes())?;
    }
    Ok(())
}

pub fn default_directory() -> Result<PathBuf> {
    if let Some(dirs) = directories::ProjectDirs::from("de", "lisp-unleashed", "graphqshell") {
        Ok(dirs.config_dir().into())
    } else {
        Err(Error::UnknownUserDirectory)
    }
}
