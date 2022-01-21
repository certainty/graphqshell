use super::views::ViewName;
pub struct Configuration {
    pub defaultView: ViewName,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            defaultView: ViewName::Introspector,
        }
    }
}
