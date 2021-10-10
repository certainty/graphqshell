use crate::frontend::tui::activity::introspector::{IntrospectorActivity, COMPONENT_LOG_BOX};
use crate::frontend::tui::components::log::{Log, LogPropsBuilder};
use tuirealm::borders::{BorderType, Borders};
use tuirealm::tui::layout::{Alignment, Constraint, Direction, Layout};
use tuirealm::tui::style::Color;
use tuirealm::PropsBuilder;

impl IntrospectorActivity {
    pub(super) fn init_view(&mut self) {
        self.view.mount(
            super::COMPONENT_LOG_BOX,
            Box::new(Log::new(
                LogPropsBuilder::default()
                    .with_title("Logs", Alignment::Left)
                    .with_borders(Borders::ALL, BorderType::Plain, Color::White)
                    .build(),
            )),
        );
        self.view.active(COMPONENT_LOG_BOX)
    }

    /// create the view
    pub(super) fn view(&mut self) {
        self.context.borrow_mut().terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(1)
                .constraints(
                    [
                        Constraint::Percentage(70), // Explorer
                        Constraint::Percentage(30), // Log
                    ]
                    .as_ref(),
                )
                .split(f.size());

            // main introspector area
            let _master_chunks = Layout::default()
                .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
                .direction(Direction::Horizontal)
                .split(chunks[0]);

            // menu

            // logs
            let log_chunks = Layout::default()
                .constraints([Constraint::Length(1), Constraint::Length(10)].as_ref())
                .direction(Direction::Vertical)
                .split(chunks[1]);

            // the status bar
            let _status_bar_chunks = Layout::default()
                .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
                .direction(Direction::Horizontal)
                .horizontal_margin(1)
                .split(log_chunks[0]);

            self.view.render(super::COMPONENT_LOG_BOX, f, log_chunks[1]);
        });
    }
}
