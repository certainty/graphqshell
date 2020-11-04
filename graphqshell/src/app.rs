pub mod components;
use crate::engine;
use crate::engine::application;
use crate::engine::ui;
use tui::widgets::{Block, Borders, Widget};

// App model
#[derive(Clone)]
pub struct Model {
}

impl Model {
    pub fn new() -> Self {
        Self {  }
    }
}


// Events that might be emitted as a result of IO actions
pub enum Event {
    None,
}


pub struct GraphQShellApp {  }

impl GraphQShellApp {
    pub fn new() -> Self {
        Self {  }
    }
}

impl<W: std::io::Write> application::Application<W, Event, Model> for GraphQShellApp {
    fn initial(&self) -> (Model, Vec<application::Command<Event>>) {
        let cmd = application::command(|| {
            std::thread::sleep(std::time::Duration::from_millis(1000));
            Event::None
        });

        (Model {  }, vec!(cmd))
    }

    fn update(&self, event: &engine::Event<Event>, model: Model) -> (Model, Vec<application::Command<Event>>)  {
        let cmd = application::command(|| {
            std::thread::sleep(std::time::Duration::from_millis(1000));
            Event::None
        });

       (model, vec!(cmd))
    }

     fn view(&self, t: &mut ui::Term<W>, model: &Model) -> anyhow::Result<()> {
         t.draw(|f| {
             let size = f.size();
             let block = Block::default().title("GraphQShell").borders(Borders::ALL);
             f.render_widget(block, size);
         })?;
         Ok(())
     }
}


pub type AppEngine<W: std::io::Write> = engine::Engine<W, Event, Model, GraphQShellApp>;
