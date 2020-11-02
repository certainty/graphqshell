use crate::app;
use anyhow;
use backtrace::Backtrace;
use crossbeam_channel::{unbounded, Receiver, Sender};
use scopeguard::defer;
use std::sync::Arc;
use std::{io, panic, thread, time};

pub struct Engine<IoEventT: Send + 'static> {
    io_system: app::io::IOSystem<IoEventT>,
}

impl<IoEventT: Send + 'static> Engine<IoEventT> {
    pub fn new() -> anyhow::Result<Self> {
        let io_system = app::io::IOSystem::create()?;

        defer! { app::ui::shutdown().expect("shutdown failed"); }
        Self::set_panic_handlers()?;

        let ui = app::ui::start(app::ui::setup(io::stdout())?)?;

        Ok(Self {
            io_system: io_system,
        })
    }

    pub async fn run(&self) -> anyhow::Result<()> {
        Ok(())
    }

    fn set_panic_handlers() -> anyhow::Result<()> {
        panic::set_hook(Box::new(|e| {
            let backtrace = Backtrace::new();
            app::ui::shutdown().expect("shutdown failed inside panic");
            eprintln!("panic: {:?}\ntrace:\n{:?}", e, backtrace);
        }));

        Ok(())
    }
}
