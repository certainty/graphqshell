use iced::{executor, Column};
use iced::{Application, Color, Command, Container, Element, Length, Settings, Subscription, Text};
use iced_aw::{split, Split};

#[derive(Debug, Clone)]
pub enum Message {
    Noop,
    Introspector(IntrospectorMessage),
}

#[derive(Debug, Clone)]
pub enum Activity {
    Introspector(Introspector),
}

#[derive(Debug, Clone)]
pub struct Introspector {
    split: split::State,
}

#[derive(Debug, Clone)]
pub enum IntrospectorMessage {
    OnSplitResize(u16),
}

impl Introspector {
    pub fn update(&mut self, message: IntrospectorMessage) {
        match message {
            IntrospectorMessage::OnSplitResize(position) => self.split.set_divider_position(position),
        }
    }

    pub fn view(&mut self) -> Element<'_, self::Message> {
        let main = Container::new(Text::new("Main"))
            .width(Length::FillPortion(1))
            .height(Length::Fill)
            .center_x()
            .center_y();

        let detail = Container::new(Text::new("Detail"))
            .width(Length::FillPortion(3))
            .height(Length::Fill)
            .center_x()
            .center_y();

        Split::new(&mut self.split, main, detail, |size| {
            Message::Introspector(IntrospectorMessage::OnSplitResize(size))
        })
        .spacing(5.0)
        .into()
    }
}

pub struct App {
    activity: Activity,
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
        let introspector = Introspector {
            split: split::State::new(None, split::Axis::Vertical),
        };
        (
            Self {
                activity: Activity::Introspector(introspector),
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        "GraphQShell".to_string()
    }

    fn update(&mut self, message: Self::Message, _clipboard: &mut iced::Clipboard) -> Command<Self::Message> {
        match (&mut self.activity, message) {
            (Activity::Introspector(intro), Message::Introspector(delegate)) => {
                intro.update(delegate);
                Command::none()
            }
            _ => Command::none(),
        }
    }

    fn subscription(&self) -> Subscription<Self::Message> {
        Subscription::none()
    }

    fn view(&mut self) -> Element<'_, Self::Message> {
        let top_bar = Container::new(Text::new("Topbar").size(15))
            .width(Length::Fill)
            .height(Length::Units(5))
            .padding(5)
            .center_y();

        let status_bar = Container::new(Text::new("Status").size(10))
            .width(Length::Fill)
            .height(Length::Units(5))
            .padding(5)
            .center_y();

        let activity = match &mut self.activity {
            Activity::Introspector(introspector) => introspector.view(),
        };
        let activity_container = Container::new(activity).width(Length::Fill).height(Length::Fill);

        Column::new().spacing(20).push(top_bar).push(activity_container).push(status_bar).into()
    }

    fn mode(&self) -> iced::window::Mode {
        iced::window::Mode::Windowed
    }

    fn background_color(&self) -> Color {
        Color::WHITE
    }
}
