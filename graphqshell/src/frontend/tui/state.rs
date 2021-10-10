use crate::app::configuration::Config;
use std::rc::Rc;
use std::time::Instant;

#[derive(Debug)]
pub(crate) struct State {
    pub(crate) quit: bool,
    pub(crate) redraw: bool,
    pub(crate) last_redraw: Instant,
    pub(crate) config: Rc<Config>,
}

impl State {
    pub fn new(config: Rc<Config>) -> Self {
        Self {
            quit: false,
            redraw: false,
            last_redraw: Instant::now(),
            config,
        }
    }

    pub fn quit(&mut self) {
        self.quit = true
    }

    pub fn redraw(&mut self) {
        self.redraw = true;
    }

    pub fn reset(&mut self) {
        self.redraw = false;
        self.last_redraw = Instant::now();
    }
}
