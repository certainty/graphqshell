pub use super::{Event, Result};
use async_trait::async_trait;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::task::JoinHandle;

#[async_trait]
pub trait Handler<AppAction, AppEvent: Send> {
    async fn handle(&mut self, action: AppAction) -> anyhow::Result<Option<AppEvent>>;
}

pub struct System {
    io_thread: JoinHandle<()>,
    event_stop_capture: Arc<AtomicBool>,
}

impl System {
    pub async fn start<
        AppAction: Send + 'static,
        AppEvent: Send,
        H: Handler<AppAction, AppEvent> + Send + 'static,
    >(
        handler: H,
        mut io_rx: Receiver<AppAction>,
        event_tx: Sender<Event<AppEvent>>,
    ) -> Result<Self> {
        let stop_capture = Arc::new(AtomicBool::new(false));
        let event_stop_capture = stop_capture.clone();

        let io_thread = tokio::spawn(async move {
            let mut handler = handler;
            while !stop_capture.load(std::sync::atomic::Ordering::Relaxed) {
                if let Some(action) = io_rx.recv().await {
                    match handler.handle(action).await {
                        Ok(Some(event)) => {
                            if let Err(e) = event_tx.send(Event::App(event)).await {
                                log::error!("Failed to send event: {}", e)
                            }
                        }
                        Ok(_) => {
                            log::debug!("Io handler returned no event");
                        }
                        Err(e) => {
                            log::error!("Error in io handler: {}", e);
                        }
                    }
                }
            }
            ()
        });

        Ok(Self {
            event_stop_capture,
            io_thread,
        })
    }

    pub async fn shutdown(self) -> anyhow::Result<()> {
        self.event_stop_capture.store(true, Ordering::Relaxed);
        thread::sleep(Duration::from_millis(50));
        self.io_thread.abort();
        Ok(())
    }
}
