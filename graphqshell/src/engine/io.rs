use anyhow;
use crossbeam_channel::{unbounded, Receiver, Sender};
use std::sync::Arc;
use std::thread;

/// A Command represents code that is executed on the IO thread
///
/// Use this for every action that has synchronous or asynchronous
/// code in it. The engine makes sure that this will not block the UI.

pub trait Command<Ctx, T>: Send + Sync {
    fn call(self: Box<Self>, ctx: &Ctx) -> anyhow::Result<T>;
}

impl<Ctx: 'static, T, F: Send + Sync + FnOnce(&Ctx) -> anyhow::Result<T>> Command<Ctx, T> for F {
    fn call(self: Box<F>, ctx: &Ctx) -> anyhow::Result<T> {
        (*self)(ctx)
    }
}

pub type BoxedCommand<Ctx, T> = Box<dyn Command<Ctx, T> + Send + 'static>;

/// The IOSystem encapsualtes all background IO operations
///
pub struct IOSystem<Ctx: Send + 'static, Event: Send + 'static> {
    io_thread: thread::JoinHandle<anyhow::Result<()>>,
    io_command_tx: Arc<Sender<BoxedCommand<Ctx, Event>>>,
    io_event_rx: Arc<Receiver<Event>>,
}

impl<Ctx: Send + 'static, Event: Send + 'static> IOSystem<Ctx, Event> {
    pub fn create(ctx: Ctx) -> anyhow::Result<Self> {
        let (io_command_tx, io_command_rx) = unbounded();
        let (io_event_tx, io_event_rx) = unbounded();
        let io_command_rx_arc = Arc::new(io_command_rx);
        let io_command_tx_arc = Arc::new(io_command_tx);
        let io_event_tx_arc = Arc::new(io_event_tx);
        let io_event_rx_arc = Arc::new(io_event_rx);
        let io_thread =
            Self::start_io_thread(ctx, io_command_rx_arc.clone(), io_event_tx_arc.clone())?;

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
    pub fn dispatch_many(&self, commands: Vec<BoxedCommand<Ctx, Event>>) {
        for command in commands {
            self.dispatch(command);
        }
    }

    // Dispatch a closure to the io subsystem
    pub fn dispatch(&self, cmd: BoxedCommand<Ctx, Event>) {
        self.io_command_tx.send(cmd).unwrap();
    }

    fn start_io_thread(
        ctx: Ctx,
        io_rx: Arc<Receiver<BoxedCommand<Ctx, Event>>>,
        io_event_tx: Arc<Sender<Event>>,
    ) -> anyhow::Result<thread::JoinHandle<anyhow::Result<()>>> {
        let io_thread = thread::spawn(move || Self::handle_io_events(&ctx, &*io_rx, &*io_event_tx));
        Ok(io_thread)
    }

    #[tokio::main]
    async fn handle_io_events(
        ctx: &Ctx,
        io_rx: &Receiver<BoxedCommand<Ctx, Event>>,
        io_event_tx: &Sender<Event>,
    ) -> anyhow::Result<()> {
        while let Ok(evt) = io_rx.recv() {
            if let Ok(event) = evt.call(ctx) {
                if let Err(err) = io_event_tx.send(event) {
                    eprintln!("Failed to send IO event: {}", err);
                }
            } else {
                eprintln!("Failed to execute command");
            }
        }

        Ok(())
    }
}
