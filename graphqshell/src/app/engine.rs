pub mod io;
pub mod ui;

use crate::app;
use anyhow;
use backtrace::Backtrace;
use crossbeam_channel::{unbounded, Receiver, Sender};
use io::IOSystem;
use scopeguard::defer;
use std::sync::Arc;
use std::{io as stdio, panic, thread, time};
use ui::UISystem;

pub struct Engine<IoEventT: Send + 'static> {
    io_system: IOSystem<IoEventT>,
    ui_system: UISystem<stdio::Stdout>,
}

impl<IoEventT: Send + 'static> Engine<IoEventT> {
    pub fn new() -> anyhow::Result<Self> {
        let tick_rate = time::Duration::from_millis(100);
        let io_system = IOSystem::create()?;
        let ui_system = UISystem::create(stdio::stdout(), tick_rate)?;

        // defer! { ui_system.shutdown().expect("shutdown failed"); }
        Self::set_panic_handlers()?;

        Ok(Self {
            io_system,
            ui_system,
        })
    }

    pub async fn run(&self) -> anyhow::Result<()> {
        Ok(())
    }

    fn set_panic_handlers() -> anyhow::Result<()> {
        panic::set_hook(Box::new(|e| {
            let backtrace = Backtrace::new();
            eprintln!("panic: {:?}\ntrace:\n{:?}", e, backtrace);
        }));

        Ok(())
    }
}
