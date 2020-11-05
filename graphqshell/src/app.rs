pub mod components;
use crate::engine;
use crate::engine::application::{self, command, wrap_command, Continuation};
use crate::engine::ui;
use components::schema;
use tui::widgets::{Block, Borders};

#[derive(Clone, Debug)]
pub struct Model {
    schema: schema::Model,
}

// Events that might be emitted as a result of IO actions
pub enum Event {
    Schema(schema::Event),
    None,
}

pub struct GraphQShellApp {}

impl GraphQShellApp {
    pub fn new() -> Self {
        Self {}
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
            let block = Block::default().title("GraphQShell ").borders(Borders::ALL);
            f.render_widget(block, size);
        })?;
        Ok(())
    }
}

pub type AppEngine<W> = engine::Engine<W, Event, Model, GraphQShellApp>;
