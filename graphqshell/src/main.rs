use graphqshell::infra::termui;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    termui::main().await?;
    Ok(())
}
