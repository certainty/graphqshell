use crate::application::termui_app::action::Action;
use crate::application::termui_app::component::Component;
use crate::application::termui_app::event::Event;
use crate::application::termui_app::keymap::{KeyMap, KeymapEntry};
use crate::infra::termui::engine;
use crate::infra::termui::engine::keys::Key;
use std::io::Stdout;
use tui::backend::CrosstermBackend;
use tui::Frame;

struct CommandBar {
    active: KeyMap<Event>,
    root: KeyMap<Event>,
}

impl CommandBar {
    pub fn new(root: KeyMap<Event>) -> Self {
        CommandBar {
            active: root.clone(),
            root,
        }
    }
}

impl Component for CommandBar {
    fn draw(&mut self, frame: &mut Frame<CrosstermBackend<Stdout>>) {
        todo!()
    }

    fn update(
        &mut self,
        event: &engine::Event<Event>,
    ) -> anyhow::Result<(Option<Event>, Option<Action>)> {
        match event {
            engine::Event::Input(key) => {
                if let Key::Char(c) = key {
                    let entry = self.active.match_key(&c).cloned();

                    match entry {
                        Some(KeymapEntry::Command(_, evt)) => Ok((Some(evt), None)),
                        Some(KeymapEntry::Group(_, key_map)) => {
                            self.active = *key_map.clone();
                            Ok((None, None))
                        }
                        _ => Ok((None, None)),
                    }
                } else {
                    Ok((None, None))
                }
            }
            _ => Ok((None, None)),
        }
    }
}
