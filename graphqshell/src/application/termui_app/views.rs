use super::AppState;
use std::collections::HashMap;

// Widget names as used in the applicatioj
#[derive(Debug, Clone, PartialEq)]
pub enum WidgetName {
    CommandBar,
}

// Main views in the application
#[derive(Debug, Clone, PartialEq)]
pub enum ViewName {
    Introspector,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Visible,
    Invisible,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tile {
    visibility: Visibility,
    name: WidgetName,
}

#[repr(transparent)]
pub struct Layer(Vec<Tile>);

impl Layer {
    pub fn tile(&self, name: WidgetName) -> Option<Tile> {
        self.0.iter().find(|tile| tile.name == name).cloned()
    }

    pub fn tiles(&self) -> &[Tile] {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct View {
    focused: WidgetName,
    layers: Vec<Layer>,
}

impl View {
    pub fn introspector() -> Self {
        Self {
            focused: WidgetName::CommandBar,
            layers: vec![Layer(vec![])],
        }
    }
}

pub struct ViewState {
    views: HashMap<ViewName, View>,
    focused: Option<ViewName>,
}

impl ViewState {
    pub fn focused_view_widget(state: &AppState) -> WidgetName {
        Self::focused_view(state).focused
    }

    pub fn focused_view(state: &AppState) -> View {
        state.views.get(Self::focused_view_name(state)).unwrap_or(View::introspector())
    }

    pub fn focused_view_name(state: &AppState) -> &'static str {
        state.views.focused.map(|view| view.to_string()).unwrap_or(state.config.defaultView.to_string())
    }

    pub fn visible_widgets(state: &AppState) -> Vec<WidgetName> {
        let focused = state.views.get(&state.focused).unwrap_or(state.config.defaultView);
        let all_layers = state.views.get(&focused).map(|v| v.layers).unwrap_or(vec![]);

        all_layers
            .iter()
            .flat_map(|layer| layer.tiles())
            .filter(|tile| tile.visibility == Visibility::Visible)
            .map(|tile| tile.name)
            .collect()
    }
}
