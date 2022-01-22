use std::path::PathBuf;

use super::views::ViewName;
pub struct Configuration {
    pub defaultView: ViewName,
    pub theme_path: Option<PathBuf>,
    pub enable_log_wigdet: bool,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            defaultView: ViewName::Introspector,
            theme_path: None,
            enable_log_wigdet: true,
        }
    }
}

impl Configuration {
    pub fn load(_from: PathBuf) -> anyhow::Result<Configuration> {
        // TODO: load from file
        Ok(Configuration::default())
    }
}
