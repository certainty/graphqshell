use tui_realm_stdlib::utils::{get_block, wrap_spans};
use tuirealm::event::{Event, KeyCode};
use tuirealm::props::{
    Alignment, BlockTitle, BordersProps, Props, PropsBuilder, Table as TextTable,
};
use tuirealm::tui::{
    layout::{Corner, Rect},
    style::{Color, Style},
    widgets::{BorderType, Borders, List, ListItem, ListState},
};
use tuirealm::{Component, Frame, Msg, Payload, PropPayload, PropValue, Value};

pub struct LogPropsBuilder {
    props: Option<Props>,
}

impl Default for LogPropsBuilder {
    fn default() -> Self {
        LogPropsBuilder {
            props: Some(Props::default()),
        }
    }
}

impl PropsBuilder for LogPropsBuilder {
    fn build(&mut self) -> Props {
        self.props.take().unwrap()
    }

    fn hidden(&mut self) -> &mut Self {
        if let Some(props) = self.props.as_mut() {
            props.visible = false;
        }
        self
    }

    fn visible(&mut self) -> &mut Self {
        if let Some(props) = self.props.as_mut() {
            props.visible = true;
        }
        self
    }
}

impl From<Props> for LogPropsBuilder {
    fn from(props: Props) -> Self {
        Self { props: Some(props) }
    }
}

impl LogPropsBuilder {
    pub fn with_borders(
        &mut self,
        borders: Borders,
        variant: BorderType,
        color: Color,
    ) -> &mut Self {
        if let Some(props) = self.props.as_mut() {
            props.borders = BordersProps {
                borders,
                variant,
                color,
            }
        }
        self
    }

    pub fn with_background(&mut self, color: Color) -> &mut Self {
        if let Some(props) = self.props.as_mut() {
            props.background = color;
        }
        self
    }

    pub fn with_title<S: AsRef<str>>(&mut self, text: S, alignment: Alignment) -> &mut Self {
        if let Some(props) = self.props.as_mut() {
            props.title = Some(BlockTitle::new(text, alignment));
        }
        self
    }

    pub fn with_log(&mut self, table: TextTable) -> &mut Self {
        if let Some(props) = self.props.as_mut() {
            props
                .own
                .insert(PROP_TABLE, PropPayload::One(PropValue::Table(table)));
        }
        self
    }
}

#[derive(Clone)]
struct State {
    list_index: usize, // Index of selected element in list
    list_len: usize,   // Length of file list
    focus: bool,       // Has focus?
}

impl Default for State {
    fn default() -> Self {
        State {
            list_index: 0,
            list_len: 0,
            focus: false,
        }
    }
}

impl State {
    pub fn set_list_len(&mut self, len: usize) {
        self.list_len = len;
    }

    pub fn get_list_index(&self) -> usize {
        self.list_index
    }

    // update the list index, clamping it if required to fit the range
    pub fn update_list_index(&mut self, offset: i32) {
        let new_index = self.list_index as i32 + offset;

        // clamp the values to fit the range
        if new_index < 0 {
            self.list_index = 0
        } else if new_index >= self.list_len as i32 {
            self.list_index = self.list_len - 1;
        }
    }

    pub fn reset_list_index(&mut self) {
        self.list_index = 0;
    }
}

const PROP_TABLE: &str = "table";

pub struct Log {
    props: Props,
    states: State,
}

impl Log {
    pub fn new(props: Props) -> Self {
        let mut states: State = State::default();
        states.set_list_len(Self::table_len(&props));
        states.reset_list_index();
        Self { props, states }
    }

    fn table_len(props: &Props) -> usize {
        match props.own.get(PROP_TABLE) {
            Some(PropPayload::One(PropValue::Table(table))) => table.len(),
            _ => 0,
        }
    }
}

impl Component for Log {
    fn render(&self, render: &mut Frame, area: Rect) {
        if self.props.visible {
            let width: usize = area.width as usize - 4;

            let list_items: Vec<ListItem> = match self.props.own.get(PROP_TABLE) {
                Some(PropPayload::One(PropValue::Table(table))) => table
                    .iter()
                    .map(|row| ListItem::new(wrap_spans(row, width, &self.props)))
                    .collect(), // Make List item from TextSpan
                _ => Vec::new(),
            };

            let w = List::new(list_items)
                .block(get_block(
                    &self.props.borders,
                    self.props.title.as_ref(),
                    self.states.focus,
                ))
                .start_corner(Corner::BottomLeft)
                .highlight_symbol(">> ")
                .style(Style::default().bg(self.props.background))
                .highlight_style(Style::default().add_modifier(self.props.modifiers));

            let mut state: ListState = ListState::default();
            state.select(Some(self.states.list_index));
            render.render_stateful_widget(w, area, &mut state);
        }
    }

    fn update(&mut self, props: Props) -> Msg {
        self.props = props;
        self.states.set_list_len(Self::table_len(&self.props));
        self.states.reset_list_index();
        Msg::None
    }

    fn get_props(&self) -> Props {
        self.props.clone()
    }

    fn on(&mut self, ev: Event) -> Msg {
        if let Event::Key(key) = ev {
            match key.code {
                KeyCode::Up => {
                    self.states.update_list_index(1);
                    Msg::None
                }
                KeyCode::Down => {
                    self.states.update_list_index(-1);
                    Msg::None
                }
                KeyCode::PageUp => {
                    self.states.update_list_index(8);
                    Msg::None
                }
                KeyCode::PageDown => {
                    self.states.update_list_index(-8);
                    Msg::None
                }
                _ => Msg::OnKey(key),
            }
        } else {
            Msg::None
        }
    }

    fn get_state(&self) -> Payload {
        Payload::One(Value::Usize(self.states.get_list_index()))
    }

    fn blur(&mut self) {
        self.states.focus = false;
    }

    fn active(&mut self) {
        self.states.focus = true;
    }
}
