pub mod components;
use crate::engine;
use crate::engine::application::{self, command, wrap_command, Continuation};
use crate::engine::ui;
use components::schema;
use tui::layout::{Constraint, Layout};
use tui::widgets::{Block, Borders};

#[derive(Clone, Debug)]
pub struct Model {
    api_url: url::Url,
    schema: schema::Model,
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

impl<W: std::io::Write> application::Application<W, Event, Model> for GraphQShellApp {
    fn initial(&self) -> (Model, Vec<application::Command<Event>>) {
        let mut commands: Vec<application::Command<Event>> = Vec::new();
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
    ) -> application::Continuation<Model, Event> {
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

pub type AppEngine<W> = engine::Engine<W, Event, Model, GraphQShellApp>;
