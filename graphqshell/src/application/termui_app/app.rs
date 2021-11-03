use super::components::main;
use super::io;
use crate::infra::termui::engine::{Configuration, Engine};
use std::io::Stdout;

pub enum Action {}
#[derive(Clone)]
pub enum Event {}

pub type AppEngine = Engine<Stdout, Action, Event>;

use clap::Parser;
use log::LevelFilter;
use std::io::stdout;

#[derive(Parser)]
pub struct Opts {
    #[clap(long, short, about = "path to the configuration file")]
    config: Option<String>,
}

pub async fn main() -> anyhow::Result<()> {
    tui_logger::init_logger(LevelFilter::Debug).unwrap();
    tui_logger::set_default_level(log::LevelFilter::Debug);

    let config = Configuration::default();
    let engine = AppEngine::create(stdout(), config, io::IoHandler::new()).await?;
    engine.run::<main::Model, main::Main>().await?;
    Ok(())
}
