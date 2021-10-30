use crossterm::event::{poll, read, Event};
use std::time::Duration;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

pub(crate) struct InputHandler;

impl InputHandler {
    pub(crate) fn new() -> InputHandler {
        InputHandler {}
    }

    pub(crate) fn fetch_events(&self) -> Result<Vec<Event>> {
        let mut inbox: Vec<Event> = Vec::new();
        loop {
            match self.read_event() {
                Ok(ev_opt) => match ev_opt {
                    Some(ev) => inbox.push(ev),
                    None => break,
                },
                Err(e) => return Err(e),
            }
        }
        Ok(inbox)
    }

    pub(crate) fn read_event(&self) -> Result<Option<Event>> {
        if poll(Duration::from_millis(10))? {
            let event = read()?;
            Ok(Some(event))
        } else {
            Ok(None)
        }
    }
}
