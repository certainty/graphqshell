pub mod engine;
use crate::app::engine::Engine;

// App model
pub struct Model {}

// Events that might be emitted as a result of IO actions
pub enum Event {
    None,
}

pub type AppEngine = Engine<Event>;
