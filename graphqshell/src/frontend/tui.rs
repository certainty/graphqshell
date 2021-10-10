pub(crate) mod context;
pub(crate) mod input;
pub(crate) mod keymap;
pub(crate) mod state;

use crate::app::configuration::Config;
use crate::frontend::tui::keymap::MSG_KEY_ESC;
use std::rc::Rc;
use std::thread::sleep;
use thiserror::Error;
use tuirealm::tui::layout::{Constraint, Direction, Layout};
use tuirealm::{Msg, View};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    InputHandlerError(#[from] input::Error),
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

pub struct UI {
    app_config: Rc<Config>,
}

impl UI {
    pub fn new(app_config: Config) -> Self {
        Self {
            app_config: Rc::new(app_config),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        let mut ctx = context::Context::new();
        ctx.enter_alternate_screen();
        ctx.clear_screen();

        let redraw_rate = std::time::Duration::from_millis(50);
        let tick_rate = std::time::Duration::from_millis(
            self.app_config.application.tick_rate.num_milliseconds() as u64,
        );
        let mut main: View = View::init();
        let mut state = state::State::new(self.app_config.clone());

        while !state.quit {
            if let Some(event) = ctx.input_handler.read_event()? {
                let message = main.on(event);
                state.redraw();
                self.update(&mut state, &mut main, message);
            };

            if state.redraw || state.last_redraw.elapsed() > redraw_rate {
                self.view(&mut ctx, &main)?;
                state.reset();
            }

            sleep(tick_rate)
        }

        drop(ctx);
        Ok(())
    }

    fn update(
        &mut self,
        state: &mut state::State,
        _view: &mut View,
        message: Option<(String, Msg)>,
    ) -> Option<(String, Msg)> {
        println!("updating {:?}", message);
        match message.as_ref() {
            None => None,
            Some(msg) => match msg {
                (_, key) if key == &MSG_KEY_ESC => {
                    println!("setting state to quit");
                    state.quit();
                    None
                }
                (_, key) => {
                    println!("Non matched key: {:?}", key);
                    None
                }
                _ => None,
            },
        }
    }

    fn view(&mut self, ctx: &mut context::Context, view: &View) -> Result<()> {
        ctx.terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(1)
                .constraints(
                    [
                        Constraint::Length(3),
                        Constraint::Length(3),
                        Constraint::Length(1),
                    ]
                    .as_ref(),
                )
                .split(f.size());

            view.render("counter1", f, chunks[0]);
        })?;
        Ok(())
    }
}
