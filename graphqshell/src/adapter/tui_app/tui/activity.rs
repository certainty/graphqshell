pub mod introspector;
pub mod manager;

pub enum ExitReason {
    Quit,
}

pub trait Activity {
    fn name(&self) -> &str;

    fn on_create(&mut self);

    fn on_draw(&mut self);

    fn will_umount(&self) -> Option<&ExitReason>;

    fn on_destroy(&mut self);
}
