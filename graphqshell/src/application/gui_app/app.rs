use std::convert::TryInto;

use iced::button::{self, Button};
use iced::pane_grid::Axis;
use iced::{executor, Align, pane_grid, PaneGrid};
use iced::scrollable::{self, Scrollable};
use iced::text_input::{self, TextInput};
use iced::{
    Application, Color, Column, Command, Container, Element, Length, Row,
    Settings, Subscription, Text,
};

#[derive(Debug, Clone)]
pub enum Message {
     Noop
}

pub struct App {
    panes: pane_grid::State<Pane>
}

pub struct Pane {
    title: Option<String>,
    content: PaneContent
}

impl Pane {
    pub fn new<S: Into<String>>(content: PaneContent, title: Option<S>) -> Self {
        Self {
           title: title.map(|s| s.into()),
           content

        }
    }
}

pub enum PaneContent {
    TopBar,
    MainView,
    CommandBar,
    StatusBar
}

impl App {
    pub fn main() -> iced::Result {
        Self::run(Settings::default())
    }
}

impl Application for App {
    type Executor = executor::Default;

    type Message = self::Message;

    type Flags = ();

    fn new(_flags: Self::Flags) -> (Self, Command<Self::Message>) {
        let (mut state, top_bar) = pane_grid::State::new(Pane::new(PaneContent::TopBar, Some("Foo")));
        let (main_view, _) = state.split(Axis::Horizontal, &top_bar, Pane::new::<String>(PaneContent::MainView, None)).unwrap();
        let (status_bar, _) = state.split(Axis::Horizontal, &main_view, Pane::new::<String>(PaneContent::StatusBar, None)).unwrap();

        (Self { 
            panes: state
        }, Command::none())
    }

    fn title(&self) -> String {
        "GraphQShell".to_string()
    }

    fn update(
        &mut self,
        _message: Self::Message,
        _clipboard: &mut iced::Clipboard,
    ) -> Command<Self::Message> {
        Command::none()
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        Subscription::none()
    }

    fn view(&mut self) -> Element<'_, Self::Message> {
        let pane_grid = PaneGrid::new(&mut self.panes, |id, pane| {
            let title_bar = pane.title.clone().map(|t| {
                let title = Row::with_children(vec![Text::new(t).into()]).spacing(5);
                pane_grid::TitleBar::new(title).padding(10)
            });

            let content = match pane.content {
                PaneContent::TopBar => pane_grid::Content::new(Text::new("Top Bar")).style(style::Pane::Active),
                PaneContent::MainView => pane_grid::Content::new(Text::new("MainView")).style(style::Pane::Active),
                PaneContent::StatusBar => pane_grid::Content::new(Text::new("StatusBar")).style(style::Pane::Active),
                PaneContent::CommandBar => pane_grid::Content::new(Text::new("CommandBar")).style(style::Pane::Active),
            };

            if let Some(title_bar) = title_bar {
                content.title_bar(title_bar).style(style::TitleBar::Active)
            } else {
                content
            }
        }).spacing(10);

        Container::new(pane_grid).width(Length::Fill).height(Length::Fill).padding(10).into()
    }

    fn mode(&self) -> iced::window::Mode {
        iced::window::Mode::Windowed
    }

    fn background_color(&self) -> Color {
        Color::WHITE
    }

}

mod style {
    use iced::{container, Background, Color};

    const SURFACE: Color = Color::from_rgb(
        0xF2 as f32 / 255.0,
        0xF3 as f32 / 255.0,
        0xF5 as f32 / 255.0,
    );

    const ACTIVE: Color = Color::from_rgb(
        0x72 as f32 / 255.0,
        0x89 as f32 / 255.0,
        0xDA as f32 / 255.0,
    );

    const HOVERED: Color = Color::from_rgb(
        0x67 as f32 / 255.0,
        0x7B as f32 / 255.0,
        0xC4 as f32 / 255.0,
    );

    pub enum TitleBar {
        Active,
        Focused
    }

   impl container::StyleSheet for TitleBar {
        fn style(&self) -> container::Style {
            let pane = match self {
                Self::Active => Pane::Active,
                Self::Focused => Pane::Focused,
            }
            .style();

            container::Style {
                text_color: Some(Color::WHITE),
                background: Some(pane.border_color.into()),
                ..Default::default()
            }
        }
    }


    pub enum Pane {
        Active,
        Focused
    }

    impl container::StyleSheet for self::Pane {
        fn style(&self) -> container::Style {
            container::Style {
                background: Some(Background::Color(SURFACE)),
                border_width: 2.0,
                border_color: match self {
                    Self::Active => Color::from_rgb(0.7, 0.7, 0.7),
                    Self::Focused => Color::BLACK,
                },
                ..Default::default()
            }
        }
    }
}
