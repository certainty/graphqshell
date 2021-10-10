use crate::app::configuration::Config;
use crate::frontend::tui::activity::introspector::IntrospectorActivity;
use crate::frontend::tui::activity::Activity;
use crate::frontend::tui::context::Context;
use std::cell::RefCell;
use std::rc::Rc;
use std::thread::sleep;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
}

// All available activities are enumerated here
#[derive(Clone, Debug)]
pub enum NextActivity {
    NoActivity,
    Introspector,
}
// The Manager takes care of invoking the correct activities
// and is the main entry-point of the UI. If you want to follow the
// flow of the frontend, start here.
pub struct Manager {
    config: Rc<Config>,
    context: Rc<RefCell<Context>>,
}

impl Manager {
    pub fn new(config: Rc<Config>) -> Self {
        Self {
            context: Rc::from(RefCell::from(Context::new())),
            config,
        }
    }

    pub fn run(&mut self, initial_activity: NextActivity) -> Result<()> {
        let mut current_activity = initial_activity;
        let tick_rate = std::time::Duration::from_millis(
            self.config.application.tick_rate.num_milliseconds() as u64,
        );

        self.context.borrow_mut().enter_alternate_screen();
        loop {
            match current_activity {
                NextActivity::NoActivity => break,
                NextActivity::Introspector => {
                    let mut activity = IntrospectorActivity::new(self.context.clone());
                    current_activity = self.run_activity(&mut activity, tick_rate)?;
                }
            }
        }
        self.prepare_exit();
        Ok(())
    }

    fn run_activity<T: Activity>(
        &mut self,
        activity: &mut T,
        tick_rate: std::time::Duration,
    ) -> Result<NextActivity> {
        log::debug!("running activity: {}", activity.name());
        let mut next_activity = NextActivity::Introspector;

        activity.on_create();
        loop {
            activity.on_draw();
            if activity.will_umount().is_some() {
                next_activity = NextActivity::NoActivity;
                break;
            } else {
                sleep(tick_rate);
            }
        }
        activity.on_destroy();

        Ok(next_activity)
    }

    pub fn prepare_exit(&mut self) {
        self.context.borrow_mut().finalize()
    }
}
