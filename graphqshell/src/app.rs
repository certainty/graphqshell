pub mod configuration;
use configuration::Config;

use clap::{AppSettings, Clap};

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
pub struct Opts {
    #[clap(long, short, about = "path to the configuration file")]
    config: Option<String>
}

pub struct Application {
}

impl Application {
    pub fn new() -> Self {
        Self  { }
    }

    pub fn run(&self, opts: &Opts) -> anyhow::Result<()> {
        let config = if let Some(path) = &opts.config {
            configuration::load(std::path::PathBuf::from(path))?
        } else {
            configuration::init_directories()?;
            configuration::load_default()?
        };

        Ok(())
    }
}

impl Default for Application {
    fn default() -> Self {
        Self::new()
    }
}