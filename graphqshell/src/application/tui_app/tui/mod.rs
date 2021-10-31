pub(crate) mod activity;
pub(crate) mod components;
pub(crate) mod context;
pub(crate) mod input;
pub(crate) mod keymap;

use crate::application::configuration::Config;
use crate::application::tui_app::tui::activity::manager;
use crate::application::tui_app::tui::activity::manager::{Manager, NextActivity};
use std::rc::Rc;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    InputHandlerError(#[from] input::Error),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    ActivityError(#[from] manager::Error),
}

pub struct TUI {
    activity_manager: Manager,
}

impl TUI {
    pub fn new(app_config: Config) -> Self {
        Self {
            activity_manager: Manager::new(Rc::new(app_config)),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        self.activity_manager.run(NextActivity::Introspector)?;
        Ok(())
    }

    pub fn prepare_exit(&mut self) {
        self.activity_manager.prepare_exit();
    }
}
