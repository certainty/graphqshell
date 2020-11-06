use crate::engine;

use anyhow;
use engine::io::BoxedCommand;
use engine::ui;
use std::io as stdio;

// re-export for convenience
pub use engine::Event;
pub type Command<Ctx, T> = BoxedCommand<Ctx, T>;

// Convenience function to create command from closure
pub fn command<Ctx: 'static, T, F>(f: F) -> Command<Ctx, T>
where
    F: (FnOnce(&Ctx) -> anyhow::Result<T>) + Send + Sync + 'static,
{
    Box::new(f)
}

pub fn wrap_command<Ctx: 'static, I: 'static, O, F>(cmd: Command<Ctx, I>, f: F) -> Command<Ctx, O>
where
    F: (FnOnce(I) -> O) + Send + Sync + 'static,
{
    command(|ctx| cmd.call(ctx).map(f))
}

/// Describe how the engine cycle shall continue
///
/// Most of the time you want to return `Continue`
/// but if you want to stop normally or abnormally you can do
/// so as well
///
pub enum Continuation<Ctx, AppModel, AppEvent> {
    Update(AppModel),
    UpdateAndPerform(AppModel, Vec<Command<Ctx, AppEvent>>),
    Perform(Vec<Command<Ctx, AppEvent>>),
    Noop,
    Stop,
    Abort,
}

pub trait Application<W: stdio::Write, Ctx: Send + 'static, AppEvent: Send + 'static, AppModel> {
    fn create_context(&self) -> Ctx;

    fn initial(&self) -> (AppModel, Vec<Command<Ctx, AppEvent>>);

    fn update(
        &self,
        event: &Event<AppEvent>,
        model: &AppModel,
    ) -> Continuation<Ctx, AppModel, AppEvent>;
    fn view(&self, t: &mut ui::Term<W>, model: &AppModel) -> anyhow::Result<()>;
}
