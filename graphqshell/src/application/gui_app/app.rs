use iced::button::{self, Button};
use iced::executor;
use iced::scrollable::{self, Scrollable};
use iced::text_input::{self, TextInput};
use iced::{
    Application, Color, Column, Command, Container, Element, Length, Row,
    Settings, Subscription, Text,
};

#[derive(Debug, Clone)]
 pub enum Message {
}

pub struct App {

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
        todo!()
    }

    fn title(&self) -> String {
        todo!()
    }

    fn update(
        &mut self,
        message: Self::Message,
        clipboard: &mut iced::Clipboard,
    ) -> Command<Self::Message> {
        todo!()
    }

    fn view(&mut self) -> Element<'_, Self::Message> {
        todo!()
    }
}
