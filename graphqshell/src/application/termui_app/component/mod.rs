mod command_bar;

use crate::application::termui_app::action::Action;
use crate::application::termui_app::event;
use crate::infra::termui::engine::{Event, UpdateResult};
use std::io::Stdout;
use tui::backend::CrosstermBackend;
use tui::Frame;

trait Component: Sized {
    fn draw(&mut self, frame: &mut Frame<CrosstermBackend<Stdout>>);
    fn update(
        &mut self,
        msg: &Event<event::Event>,
    ) -> anyhow::Result<(Option<event::Event>, Option<Action>)>;
}
