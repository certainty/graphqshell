pub mod app;
mod components;
mod io;
pub mod views;
pub mod keymap;
pub mod theme;
pub mod configuration;

use configuration::Configuration;

use self::views::ViewState;


pub struct AppState {
  config: Configuration,
  views: ViewState
}

pub enum AppEvent {

}

pub enum AppAction {

}