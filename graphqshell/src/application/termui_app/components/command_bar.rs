use crate::application::termui_app::app;
use crate::application::termui_app::app::{Action, Event};
use crate::application::termui_app::keymap::{KeyMap, KeymapEntry};
use crate::infra::termui::engine;
use crate::infra::termui::engine::keys::Key;
use crate::infra::termui::engine::ui::Frame;
use crate::infra::termui::engine::Component;
use std::io::{Stdout, Write};
use tui::backend::CrosstermBackend;
use tui::layout::Alignment;
use tui::style::{Color, Style};
use tui::text::{Span, Spans};
use tui::widgets::{Block, BorderType, Borders, Paragraph};

pub struct CommandBar;

pub(crate) struct Model {
    active: KeyMap<app::Event>,
    root: KeyMap<app::Event>,
}

impl Component<Model, app::Action, app::Event> for CommandBar {
    fn initial() -> anyhow::Result<(Model, Vec<Action>, Vec<Event>)> {
        todo!()
    }

    fn update(
        model: &mut Model,
        event: engine::Event<app::Event>,
    ) -> anyhow::Result<engine::Continuation<app::Action, app::Event>> {
        match event {
            engine::Event::KeyInput(key) => {
                if let Key::Char(c) = key {
                    let entry = model.active.match_key(&c).cloned();

                    match entry {
                        Some(KeymapEntry::Command(_, evt)) => Ok(engine::Continuation::Notify(evt)),
                        Some(KeymapEntry::Group(_, key_map)) => {
                            model.active = *key_map.clone();
                            Ok(engine::Continuation::Continue)
                        }
                        _ => Ok(engine::Continuation::Continue),
                    }
                } else {
                    Ok(engine::Continuation::Continue)
                }
            }
            _ => Ok(engine::Continuation::Continue),
        }
    }

    fn view<W: Write>(rect: &mut Frame<W>, model: &Model) -> anyhow::Result<()> {
        let size = rect.size();
        let bindings = model
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
        Ok(())
    }
}
