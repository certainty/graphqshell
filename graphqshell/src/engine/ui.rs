extern crate termion;

use anyhow::{self, Result};
use crossbeam_channel::{unbounded, Receiver, Sender};
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use std::{
    io::{self, Write},
    time,
};
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use tui::backend::TermionBackend;
use tui::Terminal;

pub type Key = termion::event::Key;

pub enum Event<I> {
    Input(I),
    Tick,
}

#[derive(Debug, Clone, Copy)]
pub struct EventsConfig {
    pub tick_rate: Duration,
}

impl Default for EventsConfig {
    fn default() -> EventsConfig {
        EventsConfig {
            tick_rate: Duration::from_millis(250),
        }
    }
}

pub type Term<W> = Terminal<TermionBackend<termion::screen::AlternateScreen<termion::raw::RawTerminal<W>>>>;

pub struct UISystem<W: Write> {
    pub term: Term<W>,
    tick_thread: thread::JoinHandle<()>,
    event_thread: thread::JoinHandle<()>,
    event_rx: Arc<Receiver<Event<Key>>>,
}

impl<W: Write> UISystem<W> {
    pub fn create(buf: W, tick_rate: time::Duration) -> Result<UISystem<W>> {
        let raw_buf = buf.into_raw_mode()?;
        let raw_buf = AlternateScreen::from(raw_buf);
        let backend = TermionBackend::new(raw_buf);
        let mut terminal = Terminal::new(backend)?;
        terminal.hide_cursor()?;
        terminal.clear()?;

        let (tx, rx) = unbounded();
        let tx_arc = Arc::new(tx);
        let rx_arc = Arc::new(rx);
        let event_thread = Self::start_event_thread(tx_arc.clone())?;
        let tick_thread = Self::start_tick_thread(tx_arc.clone(), tick_rate.clone())?;

        Ok(Self {
            term: terminal,
            event_thread: event_thread,
            tick_thread: tick_thread,
            event_rx: rx_arc,
        })
    }

    pub fn shutdown(&self) -> anyhow::Result<()> {
        Ok(())
    }

    pub fn next_event(&self) -> anyhow::Result<Event<Key>> {
        let event = self.event_rx.recv()?;
        Ok(event)
    }

    fn start_event_thread(tx: Arc<Sender<Event<Key>>>) -> anyhow::Result<thread::JoinHandle<()>> {
        let event_thread = thread::spawn(move || {
            let stdin = io::stdin();
            for evt in stdin.keys() {
                if let Ok(key) = evt {
                    if let Err(err) = tx.send(Event::Input(key)) {
                        eprintln!("{}", err);
                    }
                }
            }
        });
        Ok(event_thread)
    }

    fn start_tick_thread(
        tx: Arc<Sender<Event<Key>>>,
        tick_rate: Duration,
    ) -> anyhow::Result<thread::JoinHandle<()>> {
        let tick_thread = {
            thread::spawn(move || loop {
                if let Err(err) = tx.send(Event::Tick) {
                    eprintln!("failed to send tick event: {}", err);
                }
                thread::sleep(tick_rate);
            })
        };
        Ok(tick_thread)
    }
}
