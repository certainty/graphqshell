use crate::engine;

use anyhow;
use engine::io::BoxedCommand;
use engine::ui;
use std::io as stdio;

// re-export for convenience
pub use engine::Event;
pub type Command<T> = BoxedCommand<T>;

// Convenience function to create command from closure
pub fn command<F, T>(f: F) -> Command<T>
where
    F: (FnOnce() -> anyhow::Result<T>) + Send + Sync + 'static,
{
    Box::new(f)
}

pub fn wrap_command<I: 'static, O, F>(cmd: Command<I>, f: F) -> Command<O>
where
    F: (FnOnce(I) -> O) + Send + Sync + 'static,
{
    command(|| cmd.call().map(f))
}

/// Describe how the engine cycle shall continue
///
/// Most of the time you want to return `Continue`
/// but if you want to stop normally or abnormally you can do
/// so as well
///
pub enum Continuation<AppModel, AppEvent> {
    Update(AppModel),
    UpdateAndPerform(AppModel, Vec<Command<AppEvent>>),
    Perform(Vec<Command<AppEvent>>),
    Noop,
    Stop,
    Abort,
}

pub trait Application<W: stdio::Write, AppEvent: Send + 'static, AppModel> {
    fn initial(&self) -> (AppModel, Vec<Command<AppEvent>>);
    fn update(&self, event: &Event<AppEvent>, model: &AppModel)
        -> Continuation<AppModel, AppEvent>;
    fn view(&self, t: &mut ui::Term<W>, model: &AppModel) -> anyhow::Result<()>;
}
