use crate::application::termui_app::app;
use crate::application::termui_app::app::{Action, Event};
use crate::application::termui_app::keymap::{KeyMap, KeymapEntry};
use crate::infra::termui::engine;
use crate::infra::termui::engine::keys::Key;
use crate::infra::termui::engine::ui::Frame;
use crate::infra::termui::engine::{Component, Continuation};
use std::io::{Stdout, Write};
use tui::backend::CrosstermBackend;
use tui::layout::Alignment;
use tui::style::{Color, Style};
use tui::text::{Span, Spans};
use tui::widgets::{Block, BorderType, Borders, Paragraph};

pub struct CommandBar {
    active: KeyMap<app::Event>,
    root: KeyMap<app::Event>,
}

impl CommandBar {
    pub fn new() -> Self {
        Self {
            active: KeyMap::empty(),
            root: KeyMap::empty(),
        }
    }
}

impl Component<app::Action, app::Event> for CommandBar {
    fn initial(&self) -> Continuation<app::Action, app::Event> {
        Continuation::Continue
    }

    fn update(
        &mut self,
        event: engine::Event<app::Event>,
    ) -> engine::Continuation<app::Action, app::Event> {
        if let engine::Event::KeyInput(Key::Char(c)) = event {
            match self.active.match_key(&c).cloned() {
                Some(KeymapEntry::Command(_, evt)) => engine::Continuation::Notify(vec![evt]),
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

    fn view<W: Write>(&self, rect: &mut Frame<W>) {
        let size = rect.size();
        let bindings = self
            .active
            .bindings()
            .iter()
            .map(|(c, label)| {
                Block::default().title(vec![
                    Span::styled(format!("{}", c), Style::default().fg(Color::White)),
                    Span::styled("→", Style::default().fg(Color::Green)),
                    Span::styled(label, Style::default().fg(Color::White)),
                ])
            })
            .collect::<Vec<_>>();
    }
}
