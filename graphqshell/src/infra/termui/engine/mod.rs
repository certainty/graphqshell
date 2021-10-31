pub mod keys;

use crate::infra::termui::engine::keys::Key;
use async_trait::async_trait;
use std::io::{stdout, Stdout};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::Mutex;
use tui::backend::CrosstermBackend;
use tui::{Frame, Terminal};

pub enum Command<AppCommand> {
    App(AppCommand),
}
pub enum Event<AppEvent> {
    App(AppEvent),
    Tick,
    Input(Key),
}

pub enum UpdateResult<AppEvent, AppCommand> {
    Exit,
    Continue(Option<Event<AppEvent>>, Option<Command<AppCommand>>),
}

#[async_trait]
pub trait Application: Sized {
    type Event: Send;
    type Command: Send + Sync;

    fn init() -> anyhow::Result<(Self, Option<UpdateResult<Self::Event, Self::Command>>)>;
    fn draw(&mut self, frame: &mut Frame<CrosstermBackend<Stdout>>);
    fn update(
        &mut self,
        msg: &Event<Self::Event>,
    ) -> anyhow::Result<UpdateResult<Self::Event, Self::Command>>;
    async fn io(
        &self,
        cmd: &Command<Self::Command>,
    ) -> anyhow::Result<UpdateResult<Self::Event, Self::Command>>;
}

pub struct Engine<App: Application> {
    rx: Receiver<Event<App::Event>>,
    tx: Sender<Event<App::Event>>,
    io_tx: Sender<Command<App::Command>>,
    stop_handlers: Arc<AtomicBool>,
    initial_result: Option<UpdateResult<App::Event, App::Command>>,
    tick_rate: Duration,
    app: Arc<Mutex<App>>,
}

impl<App: Application + Send + Sync + 'static> Engine<App> {
    pub async fn run() -> anyhow::Result<()> {
        let (sync_io_tx, sync_io_rx) = tokio::sync::mpsc::channel::<Command<App::Command>>(100);
        let mut engine = Self::build(sync_io_tx)?;

        // make sure we collect user input
        engine.start_input_handler().await?;

        // start io thread and handle io events
        engine.start_io_handler(sync_io_rx).await?;

        // start the main engine loop
        engine.start_main_loop().await?;

        Ok(())
    }

    fn build(sync_io_tx: Sender<Command<App::Command>>) -> anyhow::Result<Self> {
        let (sync_tx, sync_rx) = tokio::sync::mpsc::channel::<Event<App::Event>>(100);
        let (app, initial_result) = App::init()?;

        Ok(Self {
            rx: sync_rx,
            tx: sync_tx,
            io_tx: sync_io_tx,
            stop_handlers: Arc::new(AtomicBool::default()),
            initial_result,
            tick_rate: Duration::from_millis(200),
            app: Arc::new(Mutex::new(app)),
        })
    }

    async fn send_event(&mut self, evt: Event<App::Event>) {
        if let Err(e) = self.tx.send(evt).await {
            log::error!("Oops: {}", e)
        }
    }

    async fn send_command(&mut self, cmd: Command<App::Command>) {
        if let Err(e) = self.io_tx.send(cmd).await {
            log::error!("Oops: {}", e)
        }
    }

    async fn receive_event(&mut self) -> anyhow::Result<Event<App::Event>> {
        Ok(self.rx.recv().await.unwrap_or(Event::Tick))
    }

    async fn start_input_handler(&self) -> anyhow::Result<()> {
        let event_tx = self.tx.clone();
        let event_stop_capture = self.stop_handlers.clone();
        let tick_rate = self.tick_rate.clone();

        tokio::spawn(async move {
            loop {
                // poll for tick rate duration, if no event, sent tick event.
                if crossterm::event::poll(tick_rate).unwrap() {
                    if let crossterm::event::Event::Key(key) = crossterm::event::read().unwrap() {
                        log::info!("Key pressed: {:?}", key);
                        let key = Key::from(key);
                        if let Err(err) = event_tx.send(Event::Input(key)).await {
                            log::error!("Oops!, {}", err);
                        }
                    }
                }
                if let Err(err) = event_tx.send(Event::Tick).await {
                    log::error!("Oops!, {}", err);
                }
                if event_stop_capture.load(Ordering::Relaxed) {
                    log::debug!("Shutting down input thread");
                    break;
                }
            }
        });

        Ok(())
    }

    pub async fn start_io_handler(
        &mut self,
        mut io_rx: Receiver<Command<App::Command>>,
    ) -> anyhow::Result<()> {
        let event_stop_capture = self.stop_handlers.clone();
        let app = self.app.clone();
        let tx = self.tx.clone();
        let io_tx = self.io_tx.clone();

        tokio::spawn(async move {
            while let Some(cmd) = io_rx.recv().await {
                let result = {
                    let app = app.lock().await;
                    app.io(&cmd).await
                };

                match result {
                    Ok(UpdateResult::Continue(evt, cmd)) => {
                        if let Some(e) = evt {
                            if let Err(e) = tx.send(e).await {
                                log::error!("Oops: {}", e)
                            }
                        }

                        if let Some(c) = cmd {
                            if let Err(e) = io_tx.send(c).await {
                                log::error!("Oops: {}", e)
                            }
                        }
                    }
                    Err(e) => log::error!("Oops: {}", e),
                    _ => (),
                }

                if event_stop_capture.load(Ordering::Relaxed) {
                    log::debug!("Shutting down IO thread");
                    break;
                }
            }
        });

        Ok(())
    }

    async fn start_main_loop(&mut self) -> anyhow::Result<()> {
        let stdout = stdout();
        crossterm::terminal::enable_raw_mode()?;
        let backend = CrosstermBackend::new(stdout);
        let mut terminal = Terminal::new(backend)?;
        terminal.clear()?;
        terminal.hide_cursor()?;

        // dispatch first event
        if let Some(initial_result) = self.initial_result.take() {
            match initial_result {
                UpdateResult::Continue(evt, cmd) => {
                    if let Some(e) = evt {
                        self.send_event(e).await;
                    }

                    if let Some(c) = cmd {
                        self.send_command(c).await;
                    }
                }
                _ => (), // ignore exits
            }
        }

        loop {
            // draw
            {
                let mut app = self.app.lock().await;
                terminal.draw(|rect| app.draw(rect))?;
            }

            // update
            let event = self.receive_event().await?;
            let result = {
                let mut app = self.app.lock().await;
                app.update(&event)?
            };

            match result {
                UpdateResult::Exit => break,
                UpdateResult::Continue(evt, cmd) => {
                    if let Some(e) = evt {
                        self.send_event(e).await;
                    }

                    if let Some(c) = cmd {
                        self.send_command(c).await;
                    }
                }
            }
        }

        self.stop_handlers.store(true, Ordering::Relaxed);
        terminal.clear()?;
        terminal.show_cursor()?;
        crossterm::terminal::disable_raw_mode()?;

        Ok(())
    }
}
