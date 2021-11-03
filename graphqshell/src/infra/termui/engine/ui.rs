use super::keys::Key;
use super::Event;
use super::Result;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use tokio::sync::mpsc::Sender;
use tokio::task::JoinHandle;
use tui::backend::CrosstermBackend;
use tui::Terminal;

pub type Term<W> = Terminal<CrosstermBackend<W>>;
pub type Frame<'a, W> = tui::Frame<'a, CrosstermBackend<W>>;

pub struct System<W: Write> {
    pub terminal: Term<W>,
    ui_event_thread: JoinHandle<()>,
    event_stop_capture: Arc<AtomicBool>,
}

impl<W: Write> System<W> {
    pub async fn start<AppEvent: Send>(
        buf: W,
        event_tx: Sender<Event<AppEvent>>,
        tick_rate: Duration,
    ) -> Result<Self> {
        crossterm::terminal::enable_raw_mode()?;
        let backend = CrosstermBackend::new(buf);
        let mut terminal = Terminal::new(backend)?;
        terminal.clear()?;
        terminal.hide_cursor()?;

        let stop_capture = Arc::new(AtomicBool::new(false));
        let event_stop_capture = stop_capture.clone();

        let ui_event_thread = tokio::spawn(async move {
            while !stop_capture.load(std::sync::atomic::Ordering::Relaxed) {
                // poll for tick rate duration, if no event, sent tick event.
                if crossterm::event::poll(tick_rate).unwrap() {
                    if let crossterm::event::Event::Key(key) = crossterm::event::read().unwrap() {
                        log::debug!("Key pressed: {:?}", key);
                        let key = Key::from(key);
                        if let Err(err) = event_tx.send(Event::KeyInput(key)).await {
                            log::error!("Error sending input event: {}", err);
                        }
                    }
                }
                if let Err(err) = event_tx.send(Event::Tick).await {
                    log::error!("Error sending Tick event: {}", err);
                }
            }
            ()
        });

        Ok(Self {
            terminal,
            event_stop_capture,
            ui_event_thread,
        })
    }

    pub async fn shutdown(mut self) -> anyhow::Result<()> {
        self.event_stop_capture.store(true, Ordering::Relaxed);
        self.terminal.clear()?;
        self.terminal.show_cursor()?;
        crossterm::terminal::disable_raw_mode()?;

        thread::sleep(Duration::from_millis(50));
        self.ui_event_thread.abort();
        Ok(())
    }
}
