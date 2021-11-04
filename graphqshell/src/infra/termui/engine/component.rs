use crate::infra::termui::engine::{ui, Continuation, Event};
use std::io::Write;
use tui::layout::Rect;

pub trait Component<AppAction: Send, AppEvent: Send> {
    fn initial(&self) -> Continuation<AppAction, AppEvent>;
    fn update(&mut self, event: Event<AppEvent>) -> Continuation<AppAction, AppEvent>;
    fn view<W: Write>(&self, frame: &mut ui::Frame<W>, target: Rect);
}
