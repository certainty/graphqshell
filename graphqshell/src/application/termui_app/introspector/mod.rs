use super::{app::AppState, views::WidgetName};
use crate::infra::termui::engine::ui::Frame;

pub fn draw<W: std::io::Write>(state: &AppState, widget: WidgetName, frame: &mut Frame<W>, target: tui::layout::Rect) {
    todo!();
}
