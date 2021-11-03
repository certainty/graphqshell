use graphqshell::application::termui_app2::app;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    app::main().await?;
    Ok(())
}
