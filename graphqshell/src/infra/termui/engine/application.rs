use std::io::Stdout;

use async_trait::async_trait;
use tui::backend::CrosstermBackend;
use tui::Frame;

use crate::infra::termui::engine::{Command, Event, UpdateResult};

#[async_trait]
pub trait Application: Sized {
    type Event: Send;
    type Command: Send + Sync;

    fn init() -> anyhow::Result<(Self, Option<UpdateResult<Self::Event, Self::Command>>)>;
    fn draw(&mut self, frame: &mut Frame<CrosstermBackend<Stdout>>);
    fn update(
        &mut self,
        msg: &Event<Self::Event>,
    ) -> anyhow::Result<UpdateResult<Self::Event, Self::Command>>;
    async fn io(
        &self,
        cmd: &Command<Self::Command>,
    ) -> anyhow::Result<UpdateResult<Self::Event, Self::Command>>;
}
