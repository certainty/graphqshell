use graphqshell::application::termui_app::app;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    app::main().await?;
    Ok(())
}
