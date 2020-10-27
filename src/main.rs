#[allow(dead_code)]
use std::{error::Error, io};
use termion::{input::MouseTerminal, raw::IntoRawMode, screen::AlternateScreen};
use tui::{
    backend::TermionBackend,
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Terminal,
};
use unicode_width::UnicodeWidthStr;

enum InputMode {
    Normal,
    Editing,
}

/// App holds the state of the application
struct App {
    /// Current value of the input box
    input: String,
    /// Current input mode
    input_mode: InputMode,
    /// History of recorded messages
    messages: Vec<String>,
}

impl Default for App {
    fn default() -> App {
        App {
            input: String::new(),
            input_mode: InputMode::Normal,
            messages: Vec::new(),
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // Terminal initialization
    let stdout = io::stdout().into_raw_mode()?;
    let stdout = MouseTerminal::from(stdout);
    let stdout = AlternateScreen::from(stdout);
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut rl = rustyline::Editor::<()>::new();

    // Create default app state
    let mut app = App::default();

    loop {
        // Draw UI
        terminal.draw(|f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(2)
                .constraints(
                    [
                        Constraint::Length(1),
                        Constraint::Length(3),
                        Constraint::Min(1),
                    ]
                    .as_ref(),
                )
                .split(f.size());

            //let (msg, style) = match app.input_mode {
                //InputMode::Normal => (
                    //vec![
                        //Span::raw("Press "),
                        //Span::styled("q", Style::default().add_modifier(Modifier::BOLD)),
                        //Span::raw(" to exit, "),
                        //Span::styled("e", Style::default().add_modifier(Modifier::BOLD)),
                        //Span::raw(" to start editing."),
                    //],
                    //Style::default().add_modifier(Modifier::RAPID_BLINK),
                //),
                //InputMode::Editing => (
                    //vec![
                        //Span::raw("Press "),
                        //Span::styled("Esc", Style::default().add_modifier(Modifier::BOLD)),
                        //Span::raw(" to stop editing, "),
                        //Span::styled("Enter", Style::default().add_modifier(Modifier::BOLD)),
                        //Span::raw(" to record the message"),
                    //],
                    //Style::default(),
                //),
            //};
            //let mut text = Text::from(Spans::from(msg));
            //text.patch_style(style);
            //let help_message = Paragraph::new(text);
            //f.render_widget(help_message, chunks[0]);

            let input = Paragraph::new(app.input.as_ref())
                .style(match app.input_mode {
                    InputMode::Normal => Style::default(),
                    InputMode::Editing => Style::default().fg(Color::Yellow),
                })
                .block(Block::default().borders(Borders::ALL).title("Query"));
            f.render_widget(input, chunks[1]);

            match app.input_mode {
                InputMode::Normal =>
                    // Hide the cursor. `Frame` does this by default, so we don't need to do anything here
                    {}

                InputMode::Editing => {
                    // Make the cursor visible and ask tui-rs to put it at the specified coordinates after rendering
                    f.set_cursor(
                        // Put cursor past the end of the input text
                        chunks[1].x + app.input.width() as u16 + 1,
                        // Move one line down, from the border to the input line
                        chunks[1].y + 1,
                    )
                }
            }

            let messages: Vec<ListItem> = app
                .messages
                .iter()
                .enumerate()
                .map(|(i, m)| {
                    let content = vec![Spans::from(Span::raw(format!("{}: {}", i, m)))];
                    ListItem::new(content)
                })
                .collect();
            let messages =
                List::new(messages).block(Block::default().borders(Borders::ALL).title("Result"));
            f.render_widget(messages, chunks[2]);
        })?;


        // Put the cursor back inside the input box
        //write!(
            //terminal.backend_mut(),
            //"{}",
            //termion::cursor::Goto(0, 2)
        //)?;
        terminal.set_cursor(8, 3)?;

        match rl.readline(">> ") {
            Err(_) => {
                break;
            },
            Ok(line) => {
                app.messages.push(line);
            }
        }

       }
    Ok(())
}

/*
use std::io::{self, Write};
use std::error::Error;
use termion::cursor::Goto;
use termion::input::MouseTerminal;
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use tui::backend::TermionBackend;
use tui::layout::{Constraint, Direction, Layout};
use tui::widgets::{Block, Borders, List, Widget};
use tui::text::Text;
use tui::Terminal;

/// App holds the state of the application
struct App {
    /// History of recorded messages
    messages: Vec<String>,
}

impl Default for App {
    fn default() -> App {
        App {
            messages: Vec::new(),
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // Terminal initialization
    let stdout = io::stdout().into_raw_mode()?;
    let stdout = MouseTerminal::from(stdout);
    let stdout = AlternateScreen::from(stdout);
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create default app state
    let mut app = App::default();

    let mut rl = rustyline::Editor::<()>::new();

    loop {
        // Draw UI
        terminal.draw(|mut f| {
            let chunks = Layout::default()
                .direction(Direction::Vertical)
                .margin(2)
                .constraints([Constraint::Length(3), Constraint::Min(1)].as_ref())
                .split(f.size());
            let messages = app
                .messages
                .iter()
                .enumerate()
                .map(|(i, m)| Text::raw(format!("{}: {}", i, m)));
            List::new(messages)
                .block(Block::default().borders(Borders::ALL).title("Messages"))
                .render(&mut f, chunks[1]);
        })?;

        // Put the cursor back inside the input box
        write!(
            terminal.backend_mut(),
            "{}",
            Goto(0, 2)
        )?;

        match rl.readline(">> ") {
            Err(_) => {
                break;
            },
            Ok(line) => {
                app.messages.push(line);
            }
        }
    }
    Ok(())
}

*/ 


