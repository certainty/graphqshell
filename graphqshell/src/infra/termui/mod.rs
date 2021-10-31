pub(crate) mod engine;

use crate::application::termui_app::GraphQLShellApp;
use crate::infra::termui::engine::Engine;
use log::LevelFilter;

pub async fn main() -> anyhow::Result<()> {
    tui_logger::init_logger(LevelFilter::Debug).unwrap();
    tui_logger::set_default_level(log::LevelFilter::Debug);
    Engine::<GraphQLShellApp>::run().await?;
    Ok(())
}
