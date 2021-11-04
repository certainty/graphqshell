#[derive(Debug, Clone)]
pub enum Continuation<AppAction, AppEvent> {
    Exit,
    Continue,
    Abort(String),
    Notify(Vec<AppEvent>),
    Perform(Vec<AppAction>),
    PerformAndNotify(Vec<AppAction>, Vec<AppEvent>),
}
