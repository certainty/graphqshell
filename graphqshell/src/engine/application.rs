use crate::engine;

use anyhow;
use backtrace::Backtrace;
use crossbeam_channel::{unbounded, Receiver, Sender};
use scopeguard::defer;
use std::sync::Arc;
use std::{io as stdio, panic, thread, time};
use tui::widgets::{Block, Borders, Widget};
use engine::io::BoxedCommand;
use engine::ui;

// re-export for convenience
pub use engine::Event;
pub type Command<T> = BoxedCommand<T>;

// Convenience function to create command from closure
pub fn command<F, T>(f: F) -> Command<T>
    where  F: (FnOnce() -> T) + Send + 'static
{
   Box::new(f)
}

// Convenience function to return no command
pub fn no_command<T>() -> Vec<Command<T>> {
    Vec::new()
}

pub trait Application<W: stdio::Write, AppEvent: Send + 'static, AppModel> {
    fn initial(&self) -> (AppModel, Vec<Command<AppEvent>>);
    fn update(&self, event: &Event<AppEvent>, model: AppModel) -> (AppModel, Vec<Command<AppEvent>>);
    fn view(&self, t: &mut ui::Term<W>, model: &AppModel) -> anyhow::Result<()>;
}
