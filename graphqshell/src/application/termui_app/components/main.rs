use crate::application::termui_app::app;
use crate::infra::termui::engine::ui::Frame;
use crate::infra::termui::engine::Event;
use crate::infra::termui::engine::{Component, Continuation};
use std::io::Write;
use tui::layout::{Alignment, Constraint, Direction, Layout};
use tui::style::{Color, Style};
use tui::widgets::{Block, BorderType, Borders, Paragraph};
use tui_logger::TuiLoggerWidget;

pub struct Main;
pub struct Model {
    command_bar: super::command_bar::Model,
}

impl Component<Model, app::Action, app::Event> for Main {
    fn initial() -> anyhow::Result<(Model, Vec<app::Action>, Vec<app::Event>)> {
        let (cmd_bar_model, cmd_bar_actions, cmd_bar_events) =
            super::command_bar::CommandBar::initial()?;

        Ok((
            Model {
                command_bar: cmd_bar_model,
            },
            cmd_bar_actions,
            cmd_bar_events,
        ))
    }

    fn update(
        _model: &mut Model,
        event: Event<app::Event>,
    ) -> anyhow::Result<Continuation<app::Action, app::Event>> {
        match event {
            Event::KeyInput(k) if k.is_exit() => Ok(Continuation::Exit),
            _ => Ok(Continuation::Continue),
        }
    }

    fn view<W: Write>(rect: &mut Frame<W>, _model: &Model) -> anyhow::Result<()> {
        let size = rect.size();

        // Vertical layout
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

        let title = Paragraph::new("GraphQShell")
            .style(Style::default().fg(Color::LightBlue))
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

        Ok(())
    }
}
