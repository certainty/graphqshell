use crate::application::termui_app::app;
use crate::application::termui_app::components::command_bar::CommandBar;
use crate::infra::termui::engine::ui::Frame;
use crate::infra::termui::engine::Event;
use crate::infra::termui::engine::{Component, Continuation};
use std::io::Write;
use tui::layout::{Alignment, Constraint, Direction, Layout};
use tui::style::{Color, Style};
use tui::widgets::{Block, BorderType, Borders, Paragraph};
use tui_logger::TuiLoggerWidget;

pub struct Main {
    command_bar: CommandBar,
    show_logs: bool,
}

impl Main {
    pub fn new() -> Self {
        Self {
            command_bar: CommandBar::new(),
            show_logs: true,
        }
    }

    fn build_logger_widget<'a>() -> TuiLoggerWidget<'a> {
        TuiLoggerWidget::default()
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
            .style(Style::default().fg(Color::White).bg(Color::Black))
    }
}

impl Component<app::Action, app::Event> for Main {
    fn initial(&self) -> Continuation<app::Action, app::Event> {
        Continuation::Continue
    }

    fn update(&mut self, event: Event<app::Event>) -> Continuation<app::Action, app::Event> {
        match event {
            Event::KeyInput(k) if k.is_exit() => Continuation::Exit,
            _ => Continuation::Continue,
        }
    }

    fn view<W: Write>(&self, rect: &mut Frame<W>) {
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
        if self.show_logs {
            let logger_widget = Self::build_logger_widget();
            rect.render_widget(logger_widget, chunks[3]);
        }
    }
}
