/// # Small TUI Engine
///
/// The engine module encapsulates the application architecture which solves some core problems
/// that every terminal UI has to tackle. Those are:
///
/// * A clear and simple update - render - cycle
/// * A way to execute code outside of the UI thread, so that the UI does not block while IO is performed
/// * An opinionated structure to apply to your application
/// * Testability of the whole engine
///
/// ### Basic Example
///
/// ```
/// extern crate tui;
/// use std::io::{Stdout, Write};
/// use anyhow;
/// use engine::ui::Frame;
/// use tui::widgets::{Block, Borders};
/// use graphqshell::infra::termui::engine::{self, io, Component, Continuation, Configuration, DrawableComponent};
/// use async_trait::async_trait;
/// use tui::layout::Rect;
///
/// // We have a single event for our example
///
/// pub enum Event {
///   Increment
/// }
///
/// pub enum Action { }
///
/// pub struct IoHandler {}
///
/// impl IoHandler {
///     pub fn new() -> Self {
///         Self {}
///     }
/// }
///
/// #[async_trait]
/// impl io::Handler<Action, Event> for IoHandler {
///     // the IO handler is just a NOOP in this case
///     async fn handle(&mut self, _action: Action) -> anyhow::Result<Option<Event>> {
///         Ok(None)
///     }
/// }
///
/// // The main component also represents the main model
/// pub struct Main {
///   counter: usize
/// }
///
/// impl Default for Main {
///     fn default() -> Self {
///        Self { counter: 0 }
///     }
/// }
///
/// // Implement the `App` trait for our application
/// impl App<Action, Event> for Main {
///     fn initial(&self) -> Continuation<Action, Event> {
///         Continuation::Continue
///     }
///
///     fn update(
///         &mut self,
///         event: engine::Event<Event>,
///     ) -> Continuation<Action, Event> {
///         match event {
///             engine::Event::KeyInput(k) if k.is_exit() => Continuation::Exit,
///             engine::Event::App(Event::Increment) => {
///                 self.counter += 1;
///                 Continuation::Continue
///             }
///             _ => Continuation::Continue,
///         }
///     }
///
///    fn view<W: Write>(&self, frame: &mut Frame<W>, rect: Rect) {
///        let block = Block::default().title(format!("Some cool APP <{ }>", self.counter)).borders(Borders::ALL);
///        frame.render_widget(block, rect);
///     }
///  }
///
/// // define tye app type
/// pub type AppEngine = engine::Engine<Stdout, Action, Event, IoHandler, Main>;
///
///
/// // Now we have everything to run the application
/// // #[tokio::main]
/// // pub async fn main() -> anyhow::Result<()> {
/// //    let mut engine = AppEngine::create(std::io::stdout(), Configuration::default(), IoHandler::new(), Main::default()).await?;
/// //    engine.run().await?;
/// //    Ok(())
/// //  }
///
/// ```
///
/// ### Important concepts
///
/// The engine at its core resembles the [elm architecture](https://guide.elm-lang.org/architecture/)
/// which has proven to be a rather simple and stable way to build UI applications.
///
/// Contrary to elm, the model is updated mutably. Since we're using rust this is still safe.
///
/// The engine borrows the most important concepts and also follows the overall update-render cycle.
///
/// #### IO Handler
///
/// There are conceptually two parts of the every application that uses the engine.
/// 1. App - responsible to draw the UI, manage the state of the model and react to events
/// 2. IO - the IO handler that executes IO operations outside of the UI thread
///
/// The main motivation for this separation is technical. If you have longer running
/// IO operations, you do not want to execute those in the UI thread, thus blocking the UI updates
/// making the UI unresponsive. So instead you install an IO handler which executes `Actions`,
/// in an async function.
///
/// The second reason is that it actually results in a nice separation for your application.
/// You can use the messy infrastructure details handled in the IO section while the rest of your
/// application remains pure. This also has the consequence that the application is easier to test.
/// You can provide a mock IO system if you like and decouple yourself from heavy dependencies.
///
///
/// #### App
///
/// A app represents a concrete application running on the engine.
/// The engine will take care of calling into the application
/// at appropriate times so that the TUI application can react to updates.
///
/// #### Events
///
/// Events are distinct incidents that happen during the life-time of the application.
/// The engine defines three types of events and subsumes those under the `Event<T>` enum.
/// The type parameter `T` is used to transport user defined types that are specific to the domain
/// of the application that is run on the engine. See the [Event](enum.Event.html) type for details.
///
///
/// #### Actions
///
/// Actions can be dispatched as the result of an update to the application.
/// When the update function returns `Continuation::Perform` it has to provide the action
/// to dispatch. It will be send to the IO handler, which executes it.
///
/// Actions are executed in a separate thread, in the async IO handler, and thus don't block the UI while they're running.
/// You're advised to use `Actions` if you have IO actions that are running longer.
///
/// You can also off-load CPU intensive actions as actions. However in the future there might be
/// a distinct way to handle CPU heavy work load.
///
///
use backtrace::Backtrace;
use keys::Key;
use std::io::Write;
use std::marker::PhantomData;
use std::panic;
use std::time::Duration;
use thiserror::Error;
use tokio::sync::mpsc::error::SendError;
use tokio::sync::mpsc::{Receiver, Sender};
use tui::layout::Rect;

pub mod continuation;
pub mod io;
pub mod keys;
pub mod ui;

pub use continuation::Continuation;
use continuation::Continuation::PerformAndNotify;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("Channel Error: {}", 0)]
    ChannelError(String),
    #[error(transparent)]
    Anyhow(#[from] anyhow::Error),
}

impl<T: Send + 'static> From<SendError<T>> for Error {
    fn from(e: SendError<T>) -> Self {
        Error::ChannelError(format!("{}", e))
    }
}

#[derive(Debug, Clone)]
pub struct Configuration {
    tick_rate: Duration,
    event_channel_size: usize,
    io_channel_size: usize,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            tick_rate: Duration::from_millis(200),
            event_channel_size: 100,
            io_channel_size: 100,
        }
    }
}

pub enum Event<AppEvent: Send + 'static> {
    App(AppEvent),
    KeyInput(Key),
    Tick,
}

pub trait App<AppAction: Send, AppEvent: Send + 'static> {
    fn initial() -> (Self, Continuation<AppAction, AppEvent>);
    fn update(&mut self, event: Event<AppEvent>) -> Continuation<AppAction, AppEvent>;
    fn draw<W: Write>(&self, frame: &mut ui::Frame<W>, target: Rect);
}

pub struct Engine<W: Write, AppAction: Send, AppEvent: Send + 'static, IOHandler: io::Handler<AppAction, AppEvent>, Application: App<AppAction, AppEvent>> {
    io_system: io::System,
    ui_system: ui::System<W>,
    tx: Sender<Event<AppEvent>>,
    rx: Receiver<Event<AppEvent>>,
    io_tx: Sender<AppAction>,
    _app: PhantomData<Application>,
    _phantom_io: PhantomData<IOHandler>,
}

impl<
        W: Write,
        AppEvent: Send + 'static,
        AppAction: Send + 'static,
        IOHandler: io::Handler<AppAction, AppEvent> + Send + 'static,
        Application: App<AppAction, AppEvent> + Send + 'static,
    > Engine<W, AppAction, AppEvent, IOHandler, Application>
{
    pub async fn create(buf: W, config: Configuration, ioHandler: IOHandler) -> Result<Self> {
        let (tx, rx) = tokio::sync::mpsc::channel::<Event<AppEvent>>(config.event_channel_size);
        let (io_tx, io_rx) = tokio::sync::mpsc::channel::<AppAction>(config.io_channel_size);

        let engine = Self {
            io_system: io::System::start(ioHandler, io_rx, tx.clone()).await?,
            ui_system: ui::System::start(buf, tx.clone(), config.tick_rate.clone()).await?,
            tx,
            rx,
            io_tx,
            _app: PhantomData,
            _phantom_io: PhantomData,
        };

        Ok(engine)
    }

    pub async fn run(mut self) -> Result<()> {
        Self::set_panic_handlers()?;
        let (app, continuation) = Application::initial();

        match continuation {
            Continuation::Exit => {
                self.shutdown().await?;
                return Ok(());
            }
            Continuation::Abort(msg) => {
                self.shutdown().await?;
                return Err(Error::Anyhow(anyhow::anyhow!(msg)));
            }
            other => self.handle_non_exit(other).await?,
        }

        loop {
            self.ui_system.draw(&app)?;

            let next_event = self.rx.recv().await.unwrap_or(Event::Tick);
            let continuation = app.update(next_event);

            match continuation {
                Continuation::Exit => break,
                Continuation::Abort(msg) => return Err(Error::Anyhow(anyhow::anyhow!(msg))),
                other => self.handle_non_exit(other).await?,
            }
        }

        self.shutdown().await?;
        Ok(())
    }

    async fn shutdown(self) -> anyhow::Result<()> {
        self.io_system.shutdown().await?;
        self.ui_system.shutdown().await?;
        Ok(())
    }

    async fn handle_non_exit(&mut self, continuation: Continuation<AppAction, AppEvent>) -> Result<()> {
        match continuation {
            Continuation::Continue => (),
            Continuation::Notify(events) => {
                for event in events.into_iter() {
                    self.tx.send(Event::App(event)).await?
                }
            }
            Continuation::Perform(actions) => {
                for action in actions.into_iter() {
                    self.io_tx.send(action).await?
                }
            }
            PerformAndNotify(actions, events) => {
                for event in events.into_iter() {
                    self.tx.send(Event::App(event)).await?
                }

                for action in actions.into_iter() {
                    self.io_tx.send(action).await?
                }
            }
            _ => panic!("bug"),
        }
        Ok(())
    }

    fn set_panic_handlers() -> Result<()> {
        panic::set_hook(Box::new(|e| {
            let backtrace = Backtrace::new();
            eprintln!("panic: {:?}\ntrace:\n{:?}", e, backtrace);
        }));

        Ok(())
    }
}
