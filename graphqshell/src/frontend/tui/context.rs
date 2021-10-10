use super::input;
use crossterm::event::DisableMouseCapture;
use crossterm::execute;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use std::io::{stdout, Stdout};
use tui::backend::CrosstermBackend;
use tui::Terminal;

pub struct Context {
    pub(crate) input_handler: input::InputHandler,
    pub(crate) terminal: Terminal<CrosstermBackend<Stdout>>,
}

impl Context {
    pub fn new() -> Self {
        let _ = enable_raw_mode();
        let mut stdout = stdout();
        assert!(execute!(stdout, EnterAlternateScreen).is_ok());
        Self {
            input_handler: input::InputHandler::new(),
            terminal: Terminal::new(CrosstermBackend::new(stdout)).unwrap(),
        }
    }

    pub fn enter_alternate_screen(&mut self) {
        let _ = execute!(
            self.terminal.backend_mut(),
            EnterAlternateScreen,
            DisableMouseCapture
        );
    }

    pub fn leave_alternate_screen(&mut self) {
        let _ = execute!(
            self.terminal.backend_mut(),
            LeaveAlternateScreen,
            DisableMouseCapture
        );
    }

    pub fn clear_screen(&mut self) {
        let _ = self.terminal.clear();
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        self.leave_alternate_screen();
        let _ = disable_raw_mode();
    }
}
