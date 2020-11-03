use crate::app;
use anyhow;
use backtrace::Backtrace;
use crossbeam_channel::{unbounded, Receiver, Sender};
use scopeguard::defer;
use std::sync::Arc;
use std::{io, panic, thread, time};

// TODO: put to utils
trait Task<T>: Send {
    fn call(self: Box<Self>) -> T;
}

impl<T, F: Send + FnOnce() -> T> Task<T> for F {
    fn call(self: Box<F>) -> T {
        (*self)()
    }
}

pub type IoTask<T> = Task<T> + Send + 'static;

// An IoCommand is a closure that returns a given event type
pub type IoCommand<T> = Box<IoTask<T>>;

pub struct IOSystem<IoEvent: Send + 'static> {

    io_thread: thread::JoinHandle<anyhow::Result<()>>,
    io_command_rx: Arc<Receiver<IoCommand<IoEvent>>>,
    io_command_tx: Arc<Sender<IoCommand<IoEvent>>>,
    io_event_rx: Arc<Receiver<IoEvent>>,
    io_event_tx: Arc<Sender<IoEvent>>,
}

impl<IoEvent: Send + 'static> IOSystem<IoEvent> {

    pub fn create() -> anyhow::Result<Self> {
        let (io_command_tx, io_command_rx) = unbounded();
        let (io_event_tx, io_event_rx) = unbounded();
        let io_command_rx_arc = Arc::new(io_command_rx);
        let io_command_tx_arc = Arc::new(io_command_tx);
        let io_event_tx_arc = Arc::new(io_event_tx);
        let io_event_rx_arc = Arc::new(io_event_rx);
        let io_thread = Self::start_io_thread(io_command_rx_arc.clone(), io_event_tx_arc.clone())?;

        // TODO: add CPU thread pool (with rayon)
        Ok(Self {
            io_thread: io_thread,
            io_command_rx: io_command_rx_arc,
            io_command_tx: io_command_tx_arc,
            io_event_rx: io_event_rx_arc,
            io_event_tx: io_event_tx_arc,
        })
    }

    // Get the next IO event in the queue
    pub fn next_event(&self) -> anyhow::Result<IoEvent> {
       let evt = self.io_event_rx.try_recv()?;
       Ok(evt)
    }

    // Dispatch a closure to the io subsystem
    pub fn dispatch<F>(&self, f: F)
    where
        F: (FnOnce() -> IoEvent) + Send + 'static,
    {
        let cmd = Box::new(f);
        self.io_command_tx.send(cmd).unwrap();
    }

    fn start_io_thread(
        io_rx: Arc<Receiver<IoCommand<IoEvent>>>,
        io_event_tx: Arc<Sender<IoEvent>>,
    ) -> anyhow::Result<thread::JoinHandle<anyhow::Result<()>>> {
        let io_thread = thread::spawn(move || Self::handle_io_events(&*io_rx, &*io_event_tx));
        Ok(io_thread)
    }

    #[tokio::main]
    async fn handle_io_events(
        io_rx: &Receiver<IoCommand<IoEvent>>,
        io_event_tx: &Sender<IoEvent>,
    ) -> anyhow::Result<()> {
        while let Ok(evt) = io_rx.recv() {
            let event: IoEvent = evt.call();
            // TODO: add logging on error
            io_event_tx.send(event);
        }

        Ok(())
    }
}
