use super::io;
use super::theme::Theme;
use super::views::{ViewName, ViewState};
use crate::application::termui_app::theme;
use crate::infra::termui::engine::{App, Configuration, Continuation, Engine};
use backtrace::Frame;
use clap::Parser;
use log::LevelFilter;
use std::io::stdout;
use std::io::Stdout;

#[derive(Parser)]
pub struct Opts {
    #[clap(long, short, about = "path to the configuration file")]
    config: Option<String>,
}

pub struct AppState {
    pub config: Configuration,
    pub views: ViewState,
    pub theme: Theme,
}

#[derive(Debug, Clone)]
pub enum AppEvent {
    Activate(ViewName),
    ToggleLogs,
    Quit,
}

#[derive(Debug, Clone)]
pub enum AppAction {}

pub struct TermuiApp(AppState);

impl App<AppAction, AppEvent> for TermuiApp {
    fn initial() -> (Self, Continuation<AppAction, AppEvent>) {
        todo!()
    }

    fn update(&mut self, event: crate::infra::termui::engine::Event<AppEvent>) -> Continuation<AppAction, AppEvent> {
        todo!()
    }

    fn draw<W: std::io::Write>(&self, frame: &mut Frame<W>, target: tui::layout::Rect) {
        let visible = ViewState::visible_widgets
    }
}

pub type AppEngine = Engine<Stdout, AppAction, AppEvent, io::IoHandler, TermuiApp>;

impl TermuiApp {
    pub fn new() -> Self {
        Self {
            AppState {
                config: Configuration::default(),
                views: ViewState::default(),
                theme: theme::default(),
            },

        }
    }

    pub async fn main() -> anyhow::Result<()> {
        tui_logger::init_logger(LevelFilter::Debug).unwrap();
        tui_logger::set_default_level(log::LevelFilter::Debug);

        let theme = theme::load()?;
        let config = Configuration::default();
        let engine = AppEngine::create(stdout(), config, io::IoHandler::new(), TermuiApp::new()?).await?;

        engine.run().await?;
        Ok(())
    }
}
