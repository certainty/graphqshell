use super::components::main;
use super::io;
use crate::application::termui_app::components::main::ComponentName;
use crate::application::termui_app::theme;
use crate::infra::termui::engine::{Configuration, Engine};
use clap::Parser;
use log::LevelFilter;
use std::io::stdout;
use std::io::Stdout;

#[derive(Parser)]
pub struct Opts {
    #[clap(long, short, about = "path to the configuration file")]
    config: Option<String>,
}

pub enum Action {}

#[derive(Clone)]
pub enum Event {
    Activate(ComponentName),
    ToggleLogs,
    Quit,
}

// The main application engine
pub type AppEngine = Engine<Stdout, Action, Event, io::IoHandler, main::Main>;

pub async fn main() -> anyhow::Result<()> {
    tui_logger::init_logger(LevelFilter::Debug).unwrap();
    tui_logger::set_default_level(log::LevelFilter::Debug);

    let theme = theme::load()?;
    let config = Configuration::default();
    let engine = AppEngine::create(stdout(), config, io::IoHandler::new(), main::Main::new(theme)?).await?;

    engine.run().await?;
    Ok(())
}
