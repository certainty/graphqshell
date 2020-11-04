pub mod components;
use crate::engine;
use crate::engine::application::{self, Continuation};
use crate::engine::ui;
use tui::widgets::{Block, Borders};

// App model
#[derive(Clone, Debug)]
pub struct Model {
    pos: u8,
}

impl Model {
    pub fn new() -> Self {
        Self { pos: 0 }
    }
}

// Events that might be emitted as a result of IO actions
pub enum Event {
    Rotate,
    Quit,
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
        let cmd = application::command(|| {
            std::thread::sleep(std::time::Duration::from_millis(1000));
            Ok(Event::Rotate)
        });

        (Model::new(), vec![cmd])
    }

    fn update(
        &self,
        event: &engine::Event<Event>,
        model: &Model,
    ) -> application::Continuation<Model, Event> {
        let cmd = application::command(|| {
            std::thread::sleep(std::time::Duration::from_millis(1000));
            Ok(Event::Rotate)
        });

        match event {
            engine::Event::App(Event::Rotate) => Continuation::UpdateAndPerform(
                Model {
                    pos: (model.pos + 1) % 3,
                },
                vec![cmd],
            ),

            engine::Event::Key(ui::Key::Char('q')) => Continuation::Stop,

            _ => Continuation::Perform(vec![cmd]),
        }
    }

    fn view(&self, t: &mut ui::Term<W>, model: &Model) -> anyhow::Result<()> {
        let glyph = match model.pos {
            1 => "::",
            2 => ":.",
            _ => ".:",
        };

        t.draw(|f| {
            let size = f.size();
            let block = Block::default()
                .title(format!("GraphQShell <{ }>", glyph))
                .borders(Borders::ALL);
            f.render_widget(block, size);
        })?;
        Ok(())
    }
}

pub type AppEngine<W> = engine::Engine<W, Event, Model, GraphQShellApp>;
