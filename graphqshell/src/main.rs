use clap::Parser;
use graphqshell::application::tui_app::{Application, Opts};

fn main() {
    pretty_env_logger::init();
    let opts: Opts = Opts::parse();
    let app = Application::default();
    let result = app.run(&opts);

    match result {
        Ok(()) => std::process::exit(0),
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1)
        }
    }
}
