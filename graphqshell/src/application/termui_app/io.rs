use crate::infra::termui::engine::io;
use async_trait::async_trait;

use super::app::{AppAction, AppEvent};

pub struct IoHandler {}

impl IoHandler {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl io::Handler<AppAction, AppEvent> for IoHandler {
    async fn handle(&mut self, _action: AppAction) -> anyhow::Result<Option<AppEvent>> {
        Ok(None)
    }
}
