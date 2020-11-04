use anyhow;
use crossbeam_channel::{unbounded, Receiver, Sender};
use std::sync::Arc;
use std::thread;


/// A Command represents code that is executed on the IO thread
///
/// Use this for every action that has synchronous or asynchronous
/// code in it. The engine makes sure that this will not block the UI.

pub trait Command<T>: Send {
    fn call(self: Box<Self>) -> T;

}

impl<T, F: Send + FnOnce() -> T> Command<T> for F {
    fn call(self: Box<F>) -> T {
        (*self)()
    }
}


pub type BoxedCommand<T> = Box<dyn Command<T> + Send + 'static>;


/// The IOSystem encapsualtes all background IO operations
///
pub struct IOSystem<Event: Send + 'static> {
    io_thread: thread::JoinHandle<anyhow::Result<()>>,
    io_command_tx: Arc<Sender<BoxedCommand<Event>>>,
    io_event_rx: Arc<Receiver<Event>>,
}

impl<Event: Send + 'static> IOSystem<Event> {

    pub fn create() -> anyhow::Result<Self> {
        let (io_command_tx, io_command_rx) = unbounded();
        let (io_event_tx, io_event_rx) = unbounded();
        let io_command_rx_arc = Arc::new(io_command_rx);
        let io_command_tx_arc = Arc::new(io_command_tx);
        let io_event_tx_arc = Arc::new(io_event_tx);
        let io_event_rx_arc = Arc::new(io_event_rx);
        let io_thread = Self::start_io_thread(io_command_rx_arc.clone(), io_event_tx_arc.clone())?;

        Ok(Self {
            io_thread: io_thread,
            io_command_tx: io_command_tx_arc,
            io_event_rx: io_event_rx_arc,
        })
    }

    pub fn shutdown(&self) -> anyhow::Result<()> {
       // join  IO thread
       Ok(())
    }


    pub fn next_event(&self) -> anyhow::Result<Event> {
       let evt = self.io_event_rx.try_recv()?;
       Ok(evt)
    }

    // TODO: add error handling
    pub fn dispatch_many(&self, commands: Vec<BoxedCommand<Event>>) {
        for command in commands {
            self.dispatch(command);
        }
    }

    // Dispatch a closure to the io subsystem
    pub fn dispatch(&self, cmd: BoxedCommand<Event>) {
        self.io_command_tx.send(cmd).unwrap();
    }

    fn start_io_thread(
        io_rx: Arc<Receiver<BoxedCommand<Event>>>,
        io_event_tx: Arc<Sender<Event>>,
    ) -> anyhow::Result<thread::JoinHandle<anyhow::Result<()>>> {
        let io_thread = thread::spawn(move || Self::handle_io_events(&*io_rx, &*io_event_tx));
        Ok(io_thread)
    }

    #[tokio::main]
    async fn handle_io_events(
        io_rx: &Receiver<BoxedCommand<Event>>,
        io_event_tx: &Sender<Event>,
    ) -> anyhow::Result<()> {
        while let Ok(evt) = io_rx.recv() {
            let event: Event = evt.call();
            // TODO: add logging on error
            io_event_tx.send(event).unwrap();
        }

        Ok(())
    }
}
