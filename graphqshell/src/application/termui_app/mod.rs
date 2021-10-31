mod action;
mod activity;
pub mod app;
mod component;
mod event;
mod keymap;

use clap::Parser;

#[derive(Parser)]
pub struct Opts {
    #[clap(long, short, about = "path to the configuration file")]
    config: Option<String>,
}
