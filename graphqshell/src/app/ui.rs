pub mod events;
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

pub fn setup<W: Write>(buf: W) -> Result<termion::raw::RawTerminal<W>> {
    let raw_buf = buf.into_raw_mode()?;
    Ok(raw_buf)
}

pub fn start<W: Write>(buf: W) -> io::Result<UI<W>> {
    let backend = TermionBackend::new(buf);
    let mut terminal = Terminal::new(backend)?;
    terminal.hide_cursor()?;
    terminal.clear()?;

    Ok(terminal)
}

pub fn shutdown() -> Result<()> {
    Ok(())
}
