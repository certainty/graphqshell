extern crate clap;
use graphqshell::app::{AppEngine, GraphQShellApp};

use anyhow;
use clap::Clap;
use std::{io, panic, thread, time};

struct MyIoEvents {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let opts = process_command_line()?;
    let app  = GraphQShellApp::new();
    let mut engine = AppEngine::new(app, std::io::stdout())?;

    engine.run().await?;

    Ok(())
}

#[derive(Clap)]
#[clap(version = "1.0", author = "David K.")]
struct Opts {
    // The API we want to connect to
    api: String,
}

fn process_command_line() -> anyhow::Result<Opts> {
    let opts: Opts = Opts::parse();
    // TODO: validate opts
    Ok(opts)
}
