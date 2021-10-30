pub mod tui;
use crate::adapter::configuration;
use crate::adapter::tui_app::tui::TUI;
use clap::Parser;

#[derive(Parser)]
pub struct Opts {
    #[clap(long, short, about = "path to the configuration file")]
    config: Option<String>,
}

pub struct Application {}

impl Application {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, opts: &Opts) -> anyhow::Result<()> {
        let config = if let Some(path) = &opts.config {
            configuration::load(std::path::PathBuf::from(path))?
        } else {
            configuration::init_directories()?;
            configuration::load_default()?
        };

        // TODO: pass in the backend in addition to the config
        let mut ui = TUI::new(config);
        ui.run()?;

        Ok(())
    }
}

impl Default for Application {
    fn default() -> Self {
        Self::new()
    }
}
