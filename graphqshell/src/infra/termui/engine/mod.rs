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
/// use graphqshell::infra::termui::engine::{self, io, Component, Continuation, Configuration};
/// use async_trait::async_trait;
///
/// // define a very basic model that represents the application state
/// #[derive(Clone, Debug)]
/// struct Model { counter: i32 }
///
/// impl Default for Model {
///   fn default() -> Self {
///      Self { counter: 0 }
///    }
///  }
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
/// // The main component
/// pub struct Main;
///
/// // Implement the `Component` trait for our main component
/// impl<W: Write> Component<W, Model, Action, Event> for Main {
///     fn initial() -> anyhow::Result<(Model, Vec<Action>, Vec<Event>)> {
///         Ok((Model {counter: 0}, vec![], vec![]))
///     }
///
///     fn update(
///         model: &mut Model,
///         event: engine::Event<Event>,
///     ) -> anyhow::Result<Continuation<Action, Event>> {
///         match event {
///             engine::Event::App(Event::Increment) => {   
///                 model.counter += 1;
///                 Ok(Continuation::Continue)
///             }    
///             engine::Event::KeyInput(k) if k.is_exit() => Ok(Continuation::Exit),
///             _ => Ok(Continuation::Continue),
///         }
///     }
///
///     fn view(rect: &mut Frame<W>, model: &Model) -> anyhow::Result<()> {
///        let size = rect.size();
///        let block = Block::default().title(format!("Some cool APP <{ }>", model.counter)).borders(Borders::ALL);
///         rect.render_widget(block, size);
///         Ok(())
///     }
/// }
///  
/// // define tye app type
/// pub type AppEngine = engine::Engine<Stdout, Action, Event>;
///
///
/// // Now we have everything to run the application
///
/// pub async fn main() -> anyhow::Result<()> {
///     let mut engine = AppEngine::create(std::io::stdout(), Configuration::default(), IoHandler::new()).await?;
///     engine.run::<Model, Main>().await?;
///     Ok(())
/// }
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
/// There are concpetually two parts of the every application that uses the engine.
/// 1. components - responsible to draw the UI and manage the state of the model
/// 2. io - the IO handler that executes IO operations
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
/// #### Component
///
/// The application is the entry point for the engine and defines the implementations to render the ui and update
/// the model. See the [Application](application/trait.Application.html) trait's documentation for more details.
///
/// For technical reasons the `Application`is a trait but that doesn't not mean that your entire application
/// needs to use objects in the same way. In fact I encourage to use modules as the unit of structure and expose
/// the correct types and functions from there. You can wire everything together in the main `Application` implementation.
///
/// #### Model
///
/// The model is a user defined type that represets the entire state of the application. The underlying architecture
/// implements a unidirectional flow of state updates and rendering steps. There is single function that is responsible
/// for updating the model for any given `Event`. This function returns a new model and optionally a vector commands.
///
/// #### Events
///
/// Events are distinct incidents that happen during the life-time of the application.
/// The engine defines three types of events and subsumes those under the `Event<T>` enum.
/// The type parameter `T` is used to transport user defined types that are specific to the domain
/// of the application that is run on the engine. See the [Event](enum.Event.html) type for details.
///
/// The following example shows how you can use the `Event` type in the update function.
///
/// ```
/// use graphqshell::infra::termui::engine::{self, Component, Continuation, Configuration};
///
/// #[derive(Debug, Clone)]
/// struct AppModel {  }
///
/// enum Action { }
///
/// // user defined event for the app that's run
/// enum StarshipEvent {
///   Loaded,
///   Unloaded
/// }
///
///
/// fn update(model: &mut Model, event: engine::Event<StarshipEvent>) -> anyhow::Result<Continuation<Action, StarshipEvent>> {
///     match evt {
///       engine::Event::KeyInput(key) => Ok(Continuation::Continue), // A key was pressed
///       engine::Event::Tick => Ok(Continuation::Continue), // a constant signal to redraw the UI if required
///       engine::Event::App(StarshipEvent::Loaded) => Ok(Continuation::Continue), // handle custom event
///       _ => Ok(Continuation:Continue)
///     }
/// }
/// ```
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
use std::panic;
use std::time::Duration;
use thiserror::Error;
use tokio::sync::mpsc::error::SendError;
use tokio::sync::mpsc::{Receiver, Sender};

pub mod io;
pub mod keys;
pub mod ui;

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
    enable_tui_log: bool,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            tick_rate: Duration::from_millis(200),
            event_channel_size: 100,
            io_channel_size: 100,
            enable_tui_log: true,
        }
    }
}

pub enum Event<AppEvent: Send + 'static> {
    App(AppEvent),
    KeyInput(Key),
    Tick,
}

#[derive(Debug, Clone)]
pub enum Continuation<AppAction, AppEvent> {
    Exit,
    Continue,
    Notify(AppEvent),
    Perform(AppAction),
}

pub trait Component<W: Write, Model, AppAction: Send, AppEvent: Send> {
    fn initial() -> anyhow::Result<(Model, Vec<AppAction>, Vec<AppEvent>)>;
    fn update(
        model: &mut Model,
        event: Event<AppEvent>,
    ) -> anyhow::Result<Continuation<AppAction, AppEvent>>;
    fn view(t: &mut ui::Frame<W>, model: &Model) -> anyhow::Result<()>;
}

pub struct Engine<W: Write, AppAction: Send, AppEvent: Send + 'static> {
    io_system: io::System,
    ui_system: ui::System<W>,
    tx: Sender<Event<AppEvent>>,
    rx: Receiver<Event<AppEvent>>,
    io_tx: Sender<AppAction>,
}

impl<W: Write, AppEvent: Send + 'static, AppAction: Send + 'static> Engine<W, AppAction, AppEvent> {
    pub async fn create<IO: io::Handler<AppAction, AppEvent> + Send + 'static>(
        buf: W,
        config: Configuration,
        io: IO,
    ) -> Result<Self> {
        let (tx, rx) = tokio::sync::mpsc::channel::<Event<AppEvent>>(config.event_channel_size);
        let (io_tx, io_rx) = tokio::sync::mpsc::channel::<AppAction>(config.io_channel_size);

        let engine = Self {
            io_system: io::System::start(io, io_rx, tx.clone()).await?,
            ui_system: ui::System::start(buf, tx.clone(), config.tick_rate.clone()).await?,
            tx,
            rx,
            io_tx,
        };

        Ok(engine)
    }

    pub async fn run<Model, RootComponent: Component<W, Model, AppAction, AppEvent>>(
        &mut self,
    ) -> Result<Model> {
        Self::set_panic_handlers()?;
        let (mut model, actions, events) = RootComponent::initial()?;

        for event in events.into_iter() {
            self.tx.send(Event::App(event)).await?
        }

        for action in actions.into_iter() {
            self.io_tx.send(action).await?
        }

        loop {
            self.ui_system.terminal.draw(|rect| {
                if let Err(e) = RootComponent::view(rect, &model) {
                    log::error!("Error during rendering: {}", e);
                }
            })?;

            let next_event = self.rx.recv().await.unwrap_or(Event::Tick);
            match RootComponent::update(&mut model, next_event)? {
                Continuation::Exit => break,
                Continuation::Continue => (),
                Continuation::Perform(action) => self.io_tx.send(action).await?,
                Continuation::Notify(event) => self.tx.send(Event::App(event)).await?,
            }
        }

        self.io_system.shutdown()?;
        self.ui_system.shutdown()?;
        Ok(model)
    }

    fn set_panic_handlers() -> Result<()> {
        panic::set_hook(Box::new(|e| {
            let backtrace = Backtrace::new();
            eprintln!("panic: {:?}\ntrace:\n{:?}", e, backtrace);
        }));

        Ok(())
    }
}
