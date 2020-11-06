pub mod components;
use crate::engine;
use crate::engine::application::{self, wrap_command};
use crate::engine::ui;
use crate::graphql::api;
use crate::graphql::client;
use components::schema;
use tui::layout::{Constraint, Layout};
use tui::widgets::{Block, Borders};

#[derive(Clone, Debug)]
pub struct Model {
    api_url: url::Url,
    schema: schema::Model,
}

#[derive(Clone, Debug)]
pub struct CommandContext {
    graphql_api: api::Api,
}

// Events that might be emitted as a result of IO actions
pub enum Event {
    Schema(schema::Event),
    None,
}

pub struct GraphQShellApp {
    api_url: url::Url,
}

impl GraphQShellApp {
    pub fn new(api_url: url::Url) -> Self {
        Self { api_url }
    }
}

// Expose types to be used in components
pub type Command<T> = application::Command<CommandContext, T>;

pub type Continuation<M, T> = application::Continuation<CommandContext, M, T>;

// Finally we can wire up our application
impl<W: std::io::Write> application::Application<W, CommandContext, Event, Model>
    for GraphQShellApp
{
    fn create_context(&self) -> anyhow::Result<CommandContext> {
        let client =
            client::Client::new(client::ClientSettings::default(self.api_url.clone()))?;

        Ok(CommandContext {
            graphql_api: api::Api::new(client),
        })
    }

    fn initial(&self) -> (Model, Vec<Command<Event>>) {
        let mut commands: Vec<Command<Event>> = Vec::new();
        let (schema_model, schema_commands) = schema::initial();

        // adapt commands
        for cmd in schema_commands {
            commands.push(wrap_command(cmd, |evt| Event::Schema(evt)))
        }

        (
            Model {
                api_url: self.api_url.clone(),
                schema: schema_model,
            },
            commands,
        )
    }

    fn update(
        &self,
        event: &engine::Event<Event>,
        model: &Model,
    ) -> Continuation<Model, Event> {
        match event {
            engine::Event::Key(ui::Key::Char('q')) => Continuation::Stop,
            _ => Continuation::Noop,
        }
    }

    fn view(&self, t: &mut ui::Term<W>, model: &Model) -> anyhow::Result<()> {
        t.draw(|f| {
            let size = f.size();
            let chunks = Layout::default()
                .constraints([Constraint::Length(3), Constraint::Min(0)].as_ref())
                .split(size);

            let headline = Block::default().title("GraphQShell ").borders(Borders::ALL);
            let headline2 = Block::default().title("GraphQShell ").borders(Borders::ALL);
            f.render_widget(headline, chunks[0]);
            f.render_widget(headline2, chunks[1]);
        })?;

        Ok(())
    }
}

pub type AppEngine<W> = engine::Engine<W, CommandContext, Event, Model, GraphQShellApp>;
