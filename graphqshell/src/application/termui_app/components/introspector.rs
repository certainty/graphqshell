use crate::application::termui_app::app;
use crate::application::termui_app::app::{Action, Event};
use crate::application::termui_app::keymap::KeyMap;
use crate::application::termui_app::theme::Theme;
use crate::infra::termui::engine::component::DrawableComponent;
use crate::infra::termui::engine::ui::Frame;
use crate::infra::termui::engine::{self, Component, Continuation};
use std::io::Write;
use std::rc::Rc;
use tui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use tui::style::Style;
use tui::text::Spans;
use tui::widgets::{Block, Borders, Paragraph};

pub struct Introspector {
    theme: Rc<Theme>,
    keymap: KeyMap<app::Event>,
    visible: bool,
}

impl Introspector {
    pub fn new(theme: Rc<Theme>) -> Self {
        Self {
            theme,
            keymap: KeyMap::empty(),
            visible: false,
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

    fn update(&mut self, _event: engine::Event<Event>) -> Continuation<Action, Event> {
        todo!()
    }

    fn is_visible(&self) -> bool {
        self.visible
    }

    fn show(&mut self) {
        self.visible = true;
    }

    fn hide(&mut self) {
        self.visible = false;
    }
}

impl DrawableComponent for Introspector {
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
