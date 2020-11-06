use crate::app;
use crate::app::{Command, CommandContext, Continuation};
use crate::engine::application::{self, wrap_command};
use crate::engine::ui;
use crate::graphql::schema::Schema;
use anyhow;
use tui::backend::Backend;
use tui::layout::Rect;
use tui::layout::{Alignment, Constraint, Direction, Layout};
use tui::style::{Color, Modifier, Style};
use tui::text::{Span, Spans};
use tui::widgets::{Block, Borders, List, ListItem, Paragraph, Tabs};
use tui::Frame;

#[derive(Debug, Clone)]
pub struct Model {
    schema: Option<Schema>,
}

pub enum Event {
    SchemaLoaded(Schema),
    Noop,
}

pub fn initial() -> (Model, Vec<app::Command<Event>>) {
    let load_schema: application::Command<CommandContext, Event> =
        application::command(|ctx: CommandContext| {
            let schema = ctx.graphql_api.schema()?;
            Ok(())
        });

    (Model { schema: None }, vec![load_schema])
}

pub fn update(event: &Event, model: &Model) -> Continuation<Model, Event> {
    Continuation::Noop
}

pub fn view<B: Backend>(f: &mut Frame<B>, area: Rect, model: &Model) -> anyhow::Result<()> {
    let areas = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(30), Constraint::Percentage(70)].as_ref())
        .split(area);

    let fields: Vec<ListItem> = vec![ListItem::new(vec![Spans::from(".query")])];
    let fields = List::new(fields)
        .block(Block::default().borders(Borders::ALL).title("Fields"))
        .highlight_style(Style::default().add_modifier(Modifier::BOLD))
        .highlight_symbol("▸");

    f.render_widget(fields, areas[0]);

    Ok(())
}
