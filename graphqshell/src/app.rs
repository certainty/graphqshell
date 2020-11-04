pub mod components;
use crate::engine;
use crate::engine::application;

// App model
pub struct Model {
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

impl application::Application<Event, Model> for GraphQShellApp {
    fn initial(&self) -> (Model, Vec<application::Command<Event>>) {
        let cmd = application::command(|| {
            std::thread::sleep(std::time::Duration::from_millis(1000));
            Event::None
        });

        (Model {  }, vec!(cmd))
    }

    fn update(&self, event: &Event, model: Model) -> (Model, Vec<application::Command<Event>>)  {
        let cmd = application::command(|| {
            std::thread::sleep(std::time::Duration::from_millis(1000));
            Event::None
        });

       (model, vec!(cmd))
    }
}


pub type AppEngine = engine::Engine<Event, Model, GraphQShellApp>;
