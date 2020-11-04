pub mod engine;
pub mod components;
use engine::application;


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
        (Model {  }, application::no_command())
    }

    fn update(&self, event: &Event, model: Model) -> (Model, Vec<application::Command<Event>>)  {
       (model, application::no_command())
    }
}


pub type AppEngine = engine::Engine<Event, Model, GraphQShellApp>;
