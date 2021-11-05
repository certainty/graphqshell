use crate::application::termui_app::app;
use crate::application::termui_app::app::{Action, Event};
use crate::application::termui_app::keymap::KeyMap;
use crate::application::termui_app::theme::Theme;
use crate::infra::termui::engine::ui::Frame;
use crate::infra::termui::engine::{Component, Continuation};
use std::io::Write;
use std::rc::Rc;
use tui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use tui::style::Style;
use tui::text::{Span, Spans};
use tui::widgets::{Block, Borders, Paragraph};

pub struct Introspector {
    theme: Rc<Theme>,
    keymap: KeyMap<app::Event>,
}

impl Introspector {
    pub fn new(theme: Rc<Theme>) -> Self {
        Self {
            theme,
            keymap: KeyMap::empty(),
        }
    }

    pub fn keymap(&self) -> &KeyMap<app::Event> {
        &self.keymap
    }
}

impl Component<app::Action, app::Event> for Introspector {
    fn initial(&self) -> Continuation<Action, Event> {
        Continuation::Continue
    }

    fn update(&mut self, event: crate::infra::termui::engine::Event<Event>) -> Continuation<Action, Event> {
        todo!()
    }

    fn view<W: Write>(&self, frame: &mut Frame<W>, target: Rect) {
        let paragraph = Paragraph::new(Spans::from("The INTROSPECTOR"))
            .block(
                Block::default()
                    .borders(Borders::TOP | Borders::LEFT | Borders::RIGHT)
                    .style(Style::default().fg(self.theme.border)),
            )
            .alignment(Alignment::Left);

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(95)].as_ref())
            .split(target);

        frame.render_widget(paragraph, chunks[0]);
    }
}
