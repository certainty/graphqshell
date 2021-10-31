pub mod update;
pub mod view;

use crate::application::tui_app::tui::activity::{Activity, ExitReason};
use crate::application::tui_app::tui::context::Context;
use std::cell::RefCell;
use std::rc::Rc;
use tuirealm::View;

const COMPONENT_LOG_BOX: &str = "LOG_BOX";

pub struct IntrospectorActivity {
    name: String,
    view: View,
    context: Rc<RefCell<Context>>,
}

impl IntrospectorActivity {
    pub fn new(context: Rc<RefCell<Context>>) -> Self {
        Self {
            name: String::from("Introspector"),
            view: View::init(),
            context,
        }
    }
}

impl Activity for IntrospectorActivity {
    fn name(&self) -> &str {
        &self.name
    }

    fn on_create(&mut self) {
        log::debug!("initializing activity");
        self.context.borrow_mut().clear_screen();
        self.init_view();
        log::debug!("activity created")
    }

    fn on_draw(&mut self) {
        ()
    }

    fn will_umount(&self) -> Option<ExitReason> {
        None
    }

    fn on_destroy(&mut self) {
        self.context.borrow_mut().clear_screen();
    }
}
