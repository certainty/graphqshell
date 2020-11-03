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

pub enum Event<AppEvent: Send + 'static> {
    Key(ui::Key),
    Tick,
    App(AppEvent)
}


pub trait Application<AppEvent: Send + 'static, AppModel> {
    fn initial(&self) -> AppModel;
    fn update(&self, model: AppModel) -> AppModel;
}

pub struct Engine<AppEvent: Send + 'static, AppModel,  App: Application<AppEvent, AppModel>> {
    io_system: IOSystem<AppEvent>,
    ui_system: UISystem<stdio::Stdout>,
    quit: bool,
    initial_model: AppModel,
    app: App
}


impl<AppEvent: Send + 'static, AppModel: 'static, App: Application<AppEvent, AppModel>> Engine<AppEvent, AppModel, App> {
    pub fn new(app: App) -> anyhow::Result<Self> {
        let tick_rate = time::Duration::from_millis(100);
        let io_system = IOSystem::create()?;
        let ui_system = UISystem::create(stdio::stdout(), tick_rate)?;

        // defer! { ui_system.shutdown().expect("shutdown failed"); }
        Self::set_panic_handlers()?;

        Ok(Self {
            io_system: io_system,
            ui_system: ui_system,
            quit: false,
            initial_model: app.initial(),
            app: app
        })
    }

    pub async fn run(&mut self) -> anyhow::Result<()> {
        loop {
            self.draw_ui()?;

            // TODO: collect all events
            let events = self.next_events()?;

            // now update the app

            //use the events to call the update function of the app
            //update(model, events)

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

    pub fn next_events(&mut self) -> anyhow::Result<Vec<Event<AppEvent>>> {
        let mut events: Vec<Event<AppEvent>> = Vec::new();

        match self.io_system.next_event() {
            Ok(io_event) => events.push(Event::App(io_event)),
            _ => () // handle errors
        };

        match self.ui_system.next_event()? {
            ui::Event::Input(key) => match key {
                ui::Key::Char('q') => {
                    self.quit = true;
                    ()
                }
                evt => events.push(Event::Key(evt))
            },
            ui::Event::Tick => {
              events.push(Event::Tick)
            }
        };

        Ok(events)
    }

    fn set_panic_handlers() -> anyhow::Result<()> {
        panic::set_hook(Box::new(|e| {
            let backtrace = Backtrace::new();
            eprintln!("panic: {:?}\ntrace:\n{:?}", e, backtrace);
        }));

        Ok(())
    }
}
