pub mod tui;
use crate::application::configuration;
use crate::application::tui_app::tui::TUI;
use clap::Parser;

#[derive(Parser)]
pub struct Opts {
    #[clap(long, short, about = "path to the configuration file")]
    config: Option<String>,
}

pub struct Application {}

impl Default for Application {
    fn default() -> Self {
        Self::new()
    }
}

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

        let mut ui = TUI::new(config);
        ui.run()?;

        Ok(())
    }
}
