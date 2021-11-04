use crate::application::termui_app::app;
use crate::application::termui_app::keymap::KeyMap;
use crate::application::termui_app::theme::Theme;
use std::rc::Rc;

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
