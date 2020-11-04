/// # TUI Engine
///
/// The engine module encapsulates the application architecture which solves some core problems
/// that every terminal UI has to tackle. Those are:
///
/// * A clear and simple update - render - cycle
/// * A way to execute code outside of the UI thread, so that the UI does not block while IO is performed
/// * An oppiniated structure to apply to your applicatiosn
/// * Testability of the whole engine
///
/// ### Basic Example
///
/// ```
/// use anyhow;
/// use engine;
/// use engine::application;
/// use tui::widgets::{Block, Borders};
///
/// // define a very basic model that represents the application state
///
/// struct Model { counter: i32 }
///
/// // We have a single event for our example
///
/// pub enum Event {
///   Increment
/// }
///
/// // The application does only hold the initial state
/// pub struct MyApp { initial: i32  }
///
/// impl Default for MyApp {
///   fn default() -> Self {
///     Self { initial: 0 }
///   }
/// }
///
/// // Implement the engine::Application trait for our application
/// // Every app that you want to use with the engine has to do that
///
/// impl<W: std::io::Write> application::Application<W, Event, Model> for MyApp {
///    fn initial(&self) -> (Model, Vec<application::Command<Event>>) {
///       (MyModel{ self.initial }, Vec::new())
///    }
///
///    fn update(&self, event: &engine::Event<Event>, model: Model) -> (Model, Vec<application::Command<Event>>)  {
///        // simulate a longer running action
///        let cmd = application::command(|| {
///            std::thread::sleep(std::time::Duration::from_millis(1000));
///            Event::Increment
///        });
///
///       // update the model when we have an Increment event
///       let updated =  match event {
///           engine::Event::App(Event::Rotate) => Model { counter: (model.counter + 1) % 100 },
///             _ =>  model,
///        };
///
///
///       // return the updated model but also schedule the background action
///       (updated, vec!(cmd))
///    }
///
///     fn view(&self, t: &mut ui::Term<W>, model: &Model) -> anyhow::Result<()> {
///         t.draw(|f| {
///             let size = f.size();
///             let block = Block::default().title(format!("Some cool APP <{ }>", model.counter)).borders(Borders::ALL);
///             f.render_widget(block, size);
///         })?;
///         Ok(())
///     }
/// }
///
/// // define tye app type
/// pub type AppEngine<W: std::io::Write> = engine::Engine<W, Event, Model, MyApp>;
///
/// // Now run the app
///
/// #[tokio::main]
/// async fn main() -> anyhow::Result<()> {
///    let app  = MyApp::default();
///    let mut engine = AppEngine::new(app, std::io::stdout())?;
///
///    engine.run().await?;
///
///    Ok(())
/// }
///
/// ```
///
/// ### Important concepts
///
/// The engine at its core resembles the [elm architecture](https://guide.elm-lang.org/architecture/)
/// which has proven to be a rather simple and stable way to build UI applications.
///
/// The engine borrows the most imortant concepts and also follows the overall update-render cycle.
///
/// #### Update-Render-Cycle
///
/// #### Application
///
/// The application is the entry point for the engine and defines the implementations to render the ui and update
/// the model. See the [Application](application/trait.Application.html) trait's documentation for more details.
///
/// For technical reasons the `Application`is a trait but that doesn not mean that your entire application
/// needs to use objects in the same way. In fact I encourage to use modules as the unit of structure and expose
/// the correct types and functions from there. You can wire everything together in the main `Application` implementation.
///
/// #### Model
///
/// The model is a user defined type that represets the entire state of the application. The underlying architecture
/// implements a unidirectional flow of state upates and rendering steps. There is single function that is responsible
/// for updating the model for any givent `Event`. This function returns a new model and optionally a vector commands.
///
///
/// #### Events
///
/// Events are distinct incidents that happen during the life-time of the application.
/// The engine defines three types of events and subsumes those under the `Event<T>` enum.
/// The type parameter `T` is used to transport user defined types that are specific to the domain
/// of the application that is run on the engine. See the [Event](enum.Event.html) type for details.
///
/// The following example shows how you can use the `Event` type in the update funtion.
///
/// ```
/// // user defined event for the app that's run
/// enum StarshipEvent {
///   Loaded,
///   Unloaded
/// }
///
/// //...
///
/// fn update(&self, evt: &Event<StarshipEvent>, model: AppModel) -> (Model, Vec<Command<StarshipEvent>>) {
///     evt match {
///       Event::Key(key) => (), // A key was pressed
///       Event::Tick => (), // a constant signal to redraw the UI if required
///       App(StarshipEvent::Loaded) => (), // handle custom event
///     }
/// }
/// ```
///
/// #### Commands
///
/// Commands can be dispatched as the result of an update to the application.
/// The update function returns the new mode and a vector of commands.
///
/// Now what is a command exactly?
///
/// Everything that implements the [application::Command](application/trait.Command.html) trait.
/// You can think of it as a function that performs some action and returns an Event as a result.
///
/// Commands are executed in a seperate thread and thus don't block the UI while they're running.
/// You're advised to use `Command` if you have IO actions that are running longer.
///
/// You can also off-load CPU intensive actions as commands. However in the future there might be
/// a distinct way to handle CPU heavy work load.
///

pub mod io;
pub mod ui;
pub mod application;

use anyhow;
use backtrace::Backtrace;
use crossbeam_channel::{unbounded, Receiver, Sender};
use io::IOSystem;
use std::{io as stdio, panic, thread, time};
use tui::widgets::{Block, Borders, Widget};
use ui::UISystem;
use application::Application;

pub enum Event<AppEvent: Send + 'static> {
    Key(ui::Key),
    Tick,
    App(AppEvent)
}

pub struct Engine<Term: stdio::Write, AppEvent: Send + 'static, AppModel: Clone,  App: Application<Term, AppEvent, AppModel>> {
    io_system: IOSystem<AppEvent>,
    ui_system: UISystem<Term>,
    quit: bool,
    initial_model: AppModel,
    app: App
}

impl<Term: stdio::Write, AppEvent: Send + 'static, AppModel: Clone + 'static, App: Application<Term, AppEvent, AppModel>> Engine<Term, AppEvent, AppModel, App> {
    pub fn new(app: App, term: Term) -> anyhow::Result<Self> {
        let tick_rate = time::Duration::from_millis(100);
        let io_system = IOSystem::create()?;
        let ui_system = UISystem::create(term, tick_rate)?;

        // defer! { ui_system.shutdown().expect("shutdown failed"); }
        Self::set_panic_handlers()?;

        let (initial_model, commands) = app.initial();
        io_system.dispatch_many(commands);

        Ok(Self {
            io_system: io_system,
            ui_system: ui_system,
            quit: false,
            initial_model: initial_model,
            app: app
        })
    }

    pub async fn run(&mut self) -> anyhow::Result<()> {
        let mut model = self.initial_model.clone();

        // view update cycle
        loop {
            // view
            self.app.view(&mut self.ui_system.term, &model)?;

            // update
            for event in self.outstanding_events()?.iter() {
                let (updated_model, commands) = self.app.update(&event, model);
                model = updated_model;

                self.io_system.dispatch_many(commands);
            }

            // ok we're done here
            // TODO: move the decision to quit or not to the app
            if self.quit {
                break;
            }
        }

        Ok(())
    }

    fn outstanding_events(&mut self) -> anyhow::Result<Vec<Event<AppEvent>>> {
        let mut events: Vec<Event<AppEvent>> = Vec::new();

        match self.io_system.next_event() {
            Ok(io_event) => events.push(Event::App(io_event)),
            _ => () // handle errors
        };

        match self.ui_system.next_event()? {
            ui::Event::Input(key) => match key {
                ui::Key::Char('q') => {
                    self.quit = true;
                    ()
                }
                evt => events.push(Event::Key(evt))
            },
            ui::Event::Tick => {
              events.push(Event::Tick)
            }
        };

        Ok(events)
    }

    fn set_panic_handlers() -> anyhow::Result<()> {
        panic::set_hook(Box::new(|e| {
            let backtrace = Backtrace::new();
            eprintln!("panic: {:?}\ntrace:\n{:?}", e, backtrace);
        }));

        Ok(())
    }
}
