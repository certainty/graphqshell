use crate::engine::application::{self, Command, Continuation};
use crate::engine::ui;
use crate::graphql::api;
use crate::graphql::schema::Schema;
use anyhow;
use tui::widgets::{Block, Borders};

#[derive(Debug, Clone)]
pub struct Model {
    schema: Option<Schema>,
}

pub enum Event {
    SchemaLoaded(Schema),
}

pub fn initial() -> (Model, Vec<Command<Event>>) {
    (Model { schema: None }, Vec::new())
}

pub fn update(event: &Event, model: &Model) -> Continuation<Model, Event> {
    Continuation::Noop
}

pub fn view<W: std::io::Write>(t: &mut ui::Term<W>, model: &Model) -> anyhow::Result<()> {
    Ok(())
}
