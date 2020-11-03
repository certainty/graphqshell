pub mod engine;
pub mod components;
use crate::app::engine::{Engine, Application};

// App model
pub struct Model {}

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

impl Application<Event, Model> for GraphQShellApp {
    fn initial(&self) -> Model {
        Model {  }
    }

    fn update(&self, model: Model) -> Model {
       model
    }
}


pub type AppEngine = Engine<Event, Model, GraphQShellApp>;
