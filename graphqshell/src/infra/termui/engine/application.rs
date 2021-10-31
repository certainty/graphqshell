use std::io::Stdout;

use crate::infra::termui::engine::{Action, Event, UpdateResult};
use async_trait::async_trait;
use tui::backend::CrosstermBackend;
use tui::Frame;

#[async_trait]
pub trait Application: Sized {
    type Event: Send;
    type Action: Send + Sync;

    fn init() -> anyhow::Result<(Self, Option<UpdateResult<Self::Event, Self::Action>>)>;
    fn draw(&mut self, frame: &mut Frame<CrosstermBackend<Stdout>>);
    fn update(
        &mut self,
        msg: &Event<Self::Event>,
    ) -> anyhow::Result<UpdateResult<Self::Event, Self::Action>>;
    async fn io(
        &self,
        cmd: &Action<Self::Action>,
    ) -> anyhow::Result<UpdateResult<Self::Event, Self::Action>>;
}
