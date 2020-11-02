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
use tui::widgets::{Block, Borders, Widget};
use ui::UISystem;

pub struct Engine<IoEventT: Send + 'static> {
    io_system: IOSystem<IoEventT>,
    ui_system: UISystem<stdio::Stdout>,
    quit: bool,
}

impl<IoEventT: Send + 'static> Engine<IoEventT> {
    pub fn new() -> anyhow::Result<Self> {
        let tick_rate = time::Duration::from_millis(100);
        let io_system = IOSystem::create()?;
        let ui_system = UISystem::create(stdio::stdout(), tick_rate)?;

        // defer! { ui_system.shutdown().expect("shutdown failed"); }
        Self::set_panic_handlers()?;

        Ok(Self {
            io_system: io_system,
            ui_system: ui_system,
            quit: false,
        })
    }

    pub async fn run(&mut self) -> anyhow::Result<()> {
        loop {
            self.draw_ui()?;
            self.handle_events()?;

            // ok we're done here
            if self.quit {
                break;
            }
        }

        Ok(())
    }

    pub fn draw_ui(&mut self) -> anyhow::Result<()> {
        // TODO: move to ui layer and just call it here
        self.ui_system.term.draw(|f| {
            let size = f.size();
            let block = Block::default().title("GraphQShell").borders(Borders::ALL);
            f.render_widget(block, size);
        })?;

        Ok(())
    }

    pub fn handle_events(&mut self) -> anyhow::Result<()> {
        match self.ui_system.next_event()? {
            ui::Event::Input(key) => match key {
                ui::Key::Char('q') => {
                    self.quit = true;
                    return Ok(());
                }
                _ => return Ok(()),
            },
            ui::Event::Tick => {
                // handle tick
                ()
            }
        }
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
