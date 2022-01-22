use std::collections::HashMap;

use crate::infra::termui::engine::Configuration;

use super::app::AppState;

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
    pub fn new(config: &Configuration) -> Self {
        let mut views = HashMap::new();
        views.insert(ViewName::Introspector, View::introspector());

        Self {
            views,
            focused: config.defaultView.into(),
        }
    }

    pub fn focused_view_widget(state: &AppState) -> WidgetName {
        Self::focused_view(state).focused
    }

    pub fn focused_view(state: &AppState) -> View {
        state.views.get(Self::focused_view_name(state)).unwrap_or(View::introspector())
    }

    pub fn focused_view_name(state: &AppState) -> ViewName {
        state.views.focused.unwrap_or(state.config.defaultView)
    }

    pub fn visible_widgets(state: &AppState) -> Vec<WidgetName> {
        state
            .views
            .focused_view()
            .layers
            .iter()
            .flat_map(|layer| layer.tiles())
            .filter(|tile| tile.visibility == Visibility::Visible)
            .map(|tile| tile.name)
            .collect()
    }
}
