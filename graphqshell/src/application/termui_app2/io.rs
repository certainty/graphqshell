use super::app::{Action, Event};
use crate::infra::termui::engine::io;
use async_trait::async_trait;

pub struct IoHandler {}

impl IoHandler {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl io::Handler<Action, Event> for IoHandler {
    async fn handle(&mut self, _action: Action) -> anyhow::Result<Option<Event>> {
        Ok(None)
    }
}
