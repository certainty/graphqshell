extern crate termion;

use anyhow::{anyhow, bail, Result};
use std::{
    env,
    io::{self, Write},
    panic,
};
use termion::raw::IntoRawMode;
use tui::backend::TermionBackend;
use tui::Terminal;

pub type UI<W> = Terminal<TermionBackend<W>>;

pub struct UISystem<W: Write> {
    term: UI<termion::raw::RawTerminal<W>>,
}

impl<W: Write> UISystem<W> {
    pub fn create(buf: W) -> Result<UISystem<W>> {
        let raw_buf = buf.into_raw_mode()?;
        let backend = TermionBackend::new(raw_buf);
        let mut terminal = Terminal::new(backend)?;
        terminal.hide_cursor()?;
        terminal.clear()?;

        Ok(Self { term: terminal })
    }

    pub fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}
