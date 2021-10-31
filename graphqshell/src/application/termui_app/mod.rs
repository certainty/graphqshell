use crate::infra::termui::engine::{Application, Command, Event, UpdateResult};
use async_trait::async_trait;
use clap::Parser;
use std::io::Stdout;
use tui::backend::CrosstermBackend;
use tui::layout::{Alignment, Constraint, Direction, Layout};
use tui::style::{Color, Style};
use tui::widgets::{Block, BorderType, Borders, Paragraph};
use tui::Frame;
use tui_logger::TuiLoggerWidget;

#[derive(Parser)]
pub struct Opts {
    #[clap(long, short, about = "path to the configuration file")]
    config: Option<String>,
}

pub struct GraphQLShellApp {}

#[async_trait]
impl Application for GraphQLShellApp {
    type Event = ();
    type Command = ();

    fn init() -> anyhow::Result<(Self, Option<UpdateResult<Self::Event, Self::Command>>)> {
        Ok((Self {}, None))
    }

    fn draw(&mut self, rect: &mut Frame<CrosstermBackend<Stdout>>) {
        let size = rect.size();

        // Verical layout
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(
                [
                    Constraint::Length(3),
                    Constraint::Min(10),
                    Constraint::Length(3),
                    Constraint::Length(12),
                ]
                .as_ref(),
            )
            .split(size);

        let title = Paragraph::new("Plop with TUI")
            .style(Style::default().fg(Color::LightCyan))
            .alignment(Alignment::Center)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .style(Style::default().fg(Color::White))
                    .border_type(BorderType::Plain),
            );
        rect.render_widget(title, chunks[0]);

        // Logs
        let logs = TuiLoggerWidget::default()
            .style_error(Style::default().fg(Color::Red))
            .style_debug(Style::default().fg(Color::Green))
            .style_warn(Style::default().fg(Color::Yellow))
            .style_trace(Style::default().fg(Color::Gray))
            .style_info(Style::default().fg(Color::Blue))
            .block(
                Block::default()
                    .title("Logs")
                    .border_style(Style::default().fg(Color::White).bg(Color::Black))
                    .borders(Borders::ALL),
            )
            .style(Style::default().fg(Color::White).bg(Color::Black));
        rect.render_widget(logs, chunks[3]);
    }

    fn update(
        &mut self,
        msg: &Event<Self::Event>,
    ) -> anyhow::Result<UpdateResult<Self::Event, Self::Command>> {
        match msg {
            Event::Input(k) if k.is_exit() => Ok(UpdateResult::Exit),
            _ => Ok(UpdateResult::Continue(None, None)),
        }
    }

    async fn io(
        &self,
        _cmd: &Command<Self::Command>,
    ) -> anyhow::Result<UpdateResult<Self::Event, Self::Command>> {
        Ok(UpdateResult::Continue(None, None))
    }
}