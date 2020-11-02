extern crate clap;
use graphqshell::app::AppEngine;

use anyhow;
use clap::Clap;
use std::{io, panic, thread, time};

struct MyIoEvents {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let opts = process_command_line()?;
    let engine = AppEngine::new()?;

    engine.run().await?;

    // dispatch a command to the io thread
    //engine.dispatch_io_action(|| {
    //    println!("Hello from IO Action");
    //    MyIoEvents {}
    //});
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
