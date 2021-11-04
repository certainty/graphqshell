use super::components::main;
use super::io;
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
pub enum Event {}

// The main application engine
pub type AppEngine = Engine<Stdout, Action, Event, io::IoHandler, main::Main>;

pub async fn main() -> anyhow::Result<()> {
    tui_logger::init_logger(LevelFilter::Debug).unwrap();
    tui_logger::set_default_level(log::LevelFilter::Debug);

    let config = Configuration::default();
    let engine =
        AppEngine::create(stdout(), config, io::IoHandler::new(), main::Main::new()).await?;

    engine.run().await?;
    Ok(())
}
