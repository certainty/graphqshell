use crate::application::termui_app::app;
use crate::application::termui_app::components::command_bar::CommandBar;
use crate::application::termui_app::components::introspector::Introspector;
use crate::application::termui_app::keymap::{Builder, KeyMap};
use crate::application::termui_app::theme::Theme;
use crate::infra::termui::engine::keys::Key;
use crate::infra::termui::engine::ui::Frame;
use crate::infra::termui::engine::Event;
use crate::infra::termui::engine::{Component, Continuation};
use rustc_hash::FxHashMap;
use std::io::Write;
use std::rc::Rc;
use tui::layout::{Alignment, Constraint, Direction, Layout, Margin, Rect};
use tui::style::{Color, Style};
use tui::widgets::{Block, BorderType, Borders, Paragraph};
use tui_logger::TuiLoggerWidget;

#[derive(Debug, Clone, Hash, PartialEq, PartialOrd, Eq)]
pub enum ComponentName {
    Main,
    CommandBar,
    Introspector,
}

impl ToString for ComponentName {
    fn to_string(&self) -> String {
        match self {
            Self::Main => "Main".to_string(),
            Self::CommandBar => "CommandBar".to_string(),
            Self::Introspector => "Introspector".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Visibility {
    Invisible,
    Visible,
}

pub struct Main {
    show_logs: bool,
    theme: Rc<Theme>,
    command_bar: CommandBar,
    introspector: Introspector,
    active_component: ComponentName,
    visibility: FxHashMap<ComponentName, Visibility>,
}

impl Main {
    pub fn new(theme: Theme) -> anyhow::Result<Self> {
        let theme = Rc::new(theme);
        let mut main = Self {
            show_logs: false,
            command_bar: CommandBar::new(theme.clone(), KeyMap::empty()),
            introspector: Introspector::new(theme.clone()),
            theme,
            active_component: ComponentName::Main,
            visibility: FxHashMap::default(),
        };

        main.restore_main_keymap();
        Ok(main)
    }

    fn add_main_keymap(&self, builder: &mut Builder<app::Event>) {
        builder.group('g', "Global", |g| {
            g.cmd('q', "Quit", app::Event::Quit);
        });
    }

    fn restore_main_keymap(&mut self) {
        let mut builder = KeyMap::builder();
        self.add_main_keymap(&mut builder);

        builder.cmd('i', "Introspector", app::Event::Activate(ComponentName::Introspector));
        self.command_bar.set_keymap(builder.build());
    }

    fn set_component_specific_keymap(&mut self, component: ComponentName, component_specific: KeyMap<app::Event>) {
        let mut builder = KeyMap::builder();

        self.add_main_keymap(&mut builder);
        let keymap = builder.group_map(' ', component.to_string(), component_specific).build();

        self.command_bar.set_keymap(keymap);
    }

    fn is_visible(&self, component: ComponentName) -> bool {
        self.visibility.get(&component).map(|v| v == &Visibility::Visible).unwrap_or(false)
    }

    fn hide(&mut self, component: ComponentName) {
        self.visibility.insert(component.clone(), Visibility::Invisible);
    }

    fn show(&mut self, component: ComponentName) {
        self.visibility.insert(component.clone(), Visibility::Visible);
    }

    fn activate(&mut self, name: ComponentName) {
        match name {
            ComponentName::Main => {
                self.restore_main_keymap();
            }

            ComponentName::Introspector => {
                self.set_component_specific_keymap(ComponentName::Introspector, self.introspector.keymap().clone());
            }
            _ => (),
        }
    }

    fn build_logger_widget<'a>() -> TuiLoggerWidget<'a> {
        TuiLoggerWidget::default()
            .style_error(Style::default().fg(Color::Red))
            .style_debug(Style::default().fg(Color::Green))
            .style_warn(Style::default().fg(Color::Yellow))
            .style_trace(Style::default().fg(Color::Gray))
            .style_info(Style::default().fg(Color::Blue))
            .block(
                Block::default()
                    .title("Logs")
                    .border_style(Style::default().fg(Color::White).bg(Color::Black))
                    .borders(Borders::ALL),
            )
            .style(Style::default().fg(Color::White).bg(Color::Black))
    }

    fn status_bar_view(&self) -> Paragraph {
        let section_label = match &self.active_component {
            ComponentName::Main => "Main (<Space> for Menu)".to_string(),
            other => other.to_string(),
        };
        Paragraph::new(section_label)
            .style(self.theme.text)
            .alignment(Alignment::Left)
            .block(Block::default().borders(Borders::ALL))
    }

    fn top_bar_view(&self) -> Paragraph {
        Paragraph::new("http://example.com").style(self.theme.text).alignment(Alignment::Left).block(
            Block::default()
                .borders(Borders::ALL)
                .style(Style::default().fg(self.theme.border))
                .border_type(BorderType::Plain),
        )
    }
}

impl Component<app::Action, app::Event> for Main {
    fn initial(&self) -> Continuation<app::Action, app::Event> {
        Continuation::Continue
    }

    fn update(&mut self, event: Event<app::Event>) -> Continuation<app::Action, app::Event> {
        match event {
            Event::App(app::Event::Quit) => return Continuation::Exit,
            Event::KeyInput(Key::Ctrl('c')) => return Continuation::Exit,
            Event::KeyInput(k) if self.is_visible(ComponentName::CommandBar) => {
                if k == Key::Esc {
                    self.hide(ComponentName::CommandBar);
                    self.command_bar.reset();
                } else {
                    return self.command_bar.update(Event::KeyInput(k));
                }
            }
            Event::KeyInput(Key::Char(' ')) if !self.is_visible(ComponentName::CommandBar) => {
                self.show(ComponentName::CommandBar);
            }

            Event::App(app::Event::Activate(name)) => {
                self.hide(ComponentName::CommandBar);
                self.activate(name);
            }
            _ => (),
        }

        Continuation::Continue
    }

    fn view<W: Write>(&self, frame: &mut Frame<W>, target: Rect) {
        let mut top_idx = 0;
        let mut constraints = vec![Constraint::Length(3), Constraint::Min(10)];

        if self.is_visible(ComponentName::CommandBar) {
            constraints.push(Constraint::Length(4));
        }

        if self.show_logs {
            constraints.push(Constraint::Length(12));
        }

        constraints.push(Constraint::Length(3));

        let surrounding = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Min(1)].as_ref())
            .split(target);

        let main = Block::default().borders(Borders::ALL);
        frame.render_widget(main, surrounding[0]);

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints.as_ref())
            .split(surrounding[0]);

        let top_bar = self.top_bar_view();
        let status_bar = self.status_bar_view();

        frame.render_widget(top_bar, chunks[top_idx]);
        top_idx += 1;
        top_idx += 1;

        if self.is_visible(ComponentName::CommandBar) {
            self.command_bar.view(frame, chunks[top_idx].inner(&Margin { vertical: 0, horizontal: 0 }));
            top_idx += 1;
        }

        if self.show_logs {
            let logger_widget = Self::build_logger_widget();
            frame.render_widget(logger_widget, chunks[top_idx]);
            top_idx += 1;
        }

        frame.render_widget(status_bar, chunks[top_idx]);
    }
}
