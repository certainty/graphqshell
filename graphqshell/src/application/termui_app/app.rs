use super::introspector;
use super::io;
use super::theme::Theme;
use super::views::WidgetName;
use super::views::{ViewName, ViewState};
use crate::application::termui_app::theme;
use crate::infra::termui::engine::{self, App, Configuration, Continuation, Engine};
use backtrace::Frame;
use clap::Parser;
use log::LevelFilter;
use std::io::stdout;
use std::io::Stdout;
use tui::layout::Constraint;
use tui::layout::Direction;
use tui::layout::Layout;
use tui::layout::Margin;
use tui::widgets::Block;
use tui::widgets::Borders;

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
    fn initial() -> Continuation<AppAction, AppEvent> {
        todo!()
    }

    fn update(&mut self, event: crate::infra::termui::engine::Event<AppEvent>) -> Continuation<AppAction, AppEvent> {
        todo!()
    }

    // Draw renders the common components like the command bar and the logs and delegates
    // the rest of the rendering to the active view.
    fn draw<W: std::io::Write>(&self, frame: &mut Frame<W>, target: tui::layout::Rect) {
        let mut top_chunk_idx = 0;
        let mut constraints = vec![
            // status bar
            Constraint::Length(3),
            // pane for the active view
            Constraint::Min(10),
        ];

        if self.state.command_bar_is_active {
            constraints.push(Constraint::Length(4));
        }

        if self.state.show_logs {
            constraints.push(Constraint::Length(12));
        }

        constraints.push(Constraint::Length(3));

        let surrounding = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Min(1)].as_ref())
            .split(target);

        let main = Block::default().borders(Borders::ALL);
        frame.render_widget(main, surrounding[0]);

        // build the main layout
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints.as_ref())
            .split(surrounding[0]);

        let top_bar = self.top_bar_view();
        let status_bar = self.status_bar_view();

        frame.render_widget(top_bar, chunks[top_chunk_idx]);
        top_chunk_idx += 1;

        // dispatch drawing to active view
        self.draw_active_view(frame, chunks[top_chunk_idx].inner(&Margin { vertical: 0, horizontal: 0 }));
        top_chunk_idx += 1;

        if self.state.command_bar_is_active {
            command_bar::draw(frame, chunks[top_chunk_idx].inner(&Margin { vertical: 0, horizontal: 0 }));
            top_chunk_idx += 1;
        }

        if self.state.show_logs {
            let logger_widget = Self::build_logger_widget();
            frame.render_widget(logger_widget, chunks[top_chunk_idx]);
            top_chunk_idx += 1;
        }

        frame.render_widget(status_bar, chunks[top_chunk_idx]);
    }
}

pub type AppEngine = Engine<Stdout, AppAction, AppEvent, io::IoHandler, TermuiApp>;

impl TermuiApp {
    pub fn new(opts: Opts) -> anyhow::Result<Self> {
        let config = Configuration::load(opts.config)?;
        let theme = theme::load(&config.theme_path)?;
        let views = ViewState::new(&config);
        let app = Self(AppState { config, views, theme });
        Ok(app)
    }

    pub fn state(&self) -> &AppState {
        &self.0
    }

    pub fn draw_active_view(&self, frame: &mut Frame<Stdout>, target: tui::layout::Rect) {
        self.state().views.active_view();
    }

    pub async fn main() -> anyhow::Result<()> {
        tui_logger::init_logger(LevelFilter::Debug).unwrap();
        tui_logger::set_default_level(log::LevelFilter::Debug);

        let opts = Opts::parse();
        let io_handler = io::IoHandler::new();
        let application = Self::new(opts)?;
        let engine_config = engine::Configuration::default();

        let engine = AppEngine::create(stdout(), engine_config, io_handler, application).await?;

        engine.run().await?;
        Ok(())
    }
}
