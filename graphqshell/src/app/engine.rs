use std::thread;
use crossbeam_channel::{unbounded, Sender, Receiver};
use anyhow;


type IoCommand<IoEventT: Send + 'static> = Box<dyn (FnOnce() -> IoEventT) + Send + 'static>;


struct Engine<IoEventT: Send + 'static> {
    io_thread: thread::JoinHandle<anyhow::Result<()>> ,
    io_command_rx: Receiver<IoCommand<IoEventT>>,
    io_command_tx: Sender<IoCommand<IoEventT>>,
    io_event_rx: Receiver<IoEventT>,
    io_event_tx: Sender<IoEventT>,
}

impl<IoEventT: Send + 'static> Engine<IoEventT> {
    async fn new() -> anyhow::Result<Self> {
        let (io_command_tx, io_command_rx) = unbounded();
        let (io_event_tx, io_event_rx) = unbounded();
        let io_thread = Self::start_io_thread(io_command_rx, io_event_tx)?;

        Ok(Self {
            io_thread: io_thread,
            io_command_rx: io_command_rx,
            io_command_tx: io_command_tx,
            io_event_rx: io_event_rx,
            io_event_tx: io_event_tx
        })
    }

    fn dispatch_io_action<F>(&self, f: F) where F: (FnOnce() -> IoEventT) + Send + 'static {
        let cmd = Box::new(f);
        self.io_command_tx.send(cmd).unwrap();
    }

    fn start_io_thread(io_rx: Receiver<IoCommand<IoEventT>>, io_event_tx: Sender<IoEventT>) -> anyhow::Result<thread::JoinHandle<anyhow::Result<()>>> {
        let io_thread = thread::spawn(move || {
            Self::handle_io_events(io_rx, io_event_tx)
        });

        Ok(io_thread)
    }

    #[tokio::main]
    async fn handle_io_events(io_rx: Receiver<IoCommand<IoEventT>>, io_event_tx: Sender<IoEventT>) -> anyhow::Result<()> {
            while let Ok(evt) = io_rx.recv() {
                let event = (*evt)();
                io_event_tx.send(event);
            };
        Ok(())
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {

    Ok(())
}


