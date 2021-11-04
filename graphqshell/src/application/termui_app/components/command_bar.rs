use crate::application::termui_app::app;
use crate::application::termui_app::keymap::{KeyMap, KeymapEntry};
use crate::application::termui_app::theme::Theme;
use crate::infra::termui::engine;
use crate::infra::termui::engine::keys::Key;
use crate::infra::termui::engine::ui::Frame;
use crate::infra::termui::engine::{Component, Continuation};
use std::io::Write;
use std::rc::Rc;
use tui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use tui::style::Style;
use tui::text::{Span, Spans};
use tui::widgets::{Block, Borders, Paragraph};

pub struct CommandBar {
    theme: Rc<Theme>,
    active: KeyMap<app::Event>,
    root: KeyMap<app::Event>,
}

impl<'a> CommandBar {
    pub fn new(theme: Rc<Theme>, keymap: KeyMap<app::Event>) -> Self {
        Self {
            active: keymap.clone(),
            root: keymap,
            theme,
        }
    }

    pub fn reset(&mut self) {
        self.active = self.root.clone();
    }

    pub fn set_keymap(&mut self, keymap: KeyMap<app::Event>) {
        self.active = keymap.clone();
        self.root = keymap;
    }
}

impl Component<app::Action, app::Event> for CommandBar {
    fn initial(&self) -> Continuation<app::Action, app::Event> {
        Continuation::Continue
    }

    fn update(&mut self, event: engine::Event<app::Event>) -> engine::Continuation<app::Action, app::Event> {
        if let engine::Event::KeyInput(Key::Char(c)) = event {
            match self.active.match_key(&c).cloned() {
                Some(KeymapEntry::Command(_, evt)) => {
                    self.active = self.root.clone();
                    engine::Continuation::Notify(vec![evt])
                }
                Some(KeymapEntry::Group(_, key_map)) => {
                    self.active = *key_map.clone();
                    engine::Continuation::Continue
                }
                _ => engine::Continuation::Continue,
            }
        } else {
            engine::Continuation::Continue
        }
    }

    fn view<W: Write>(&self, frame: &mut Frame<W>, target: Rect) {
        let bindings = self.active.bindings();
        let mut menu_spans = Vec::with_capacity(bindings.len() * 3);

        for (c, label) in bindings {
            let key = match c {
                ' ' => "<SPACE>".to_string(),
                other => other.to_string(),
            };
            menu_spans.push(Span::styled(format!("{}", key), self.theme.commandbar_command));
            menu_spans.push(Span::styled(" → ", self.theme.commandbar_separator));
            menu_spans.push(Span::styled(label, self.theme.commandbar_description));
            menu_spans.push(Span::from("  "));
        }

        let paragraph = Paragraph::new(Spans::from(menu_spans))
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
