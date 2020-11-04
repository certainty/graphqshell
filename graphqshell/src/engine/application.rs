use crate::engine;

use anyhow;
use std::{io as stdio};
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

/// Describe how the engine cycle shall continue
///
/// Most of the time you want to return `Continue`
/// but if you want to stop normally or abnormally you can do
/// so as well
///
pub enum Continuation<AppModel, AppEvent> {
    Continue(AppModel, Vec<Command<AppEvent>>),
    Stop,
    Abort
}

pub trait Application<W: stdio::Write, AppEvent: Send + 'static, AppModel> {
    fn initial(&self) -> (AppModel, Vec<Command<AppEvent>>);
    fn update(&self, event: &Event<AppEvent>, model: AppModel) -> Continuation<AppModel, AppEvent>;
    fn view(&self, t: &mut ui::Term<W>, model: &AppModel) -> anyhow::Result<()>;
}
