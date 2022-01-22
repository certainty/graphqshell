use std::path::PathBuf;

use tui::style::{Color, Modifier, Style};

pub struct Theme {
    pub foreground: Color,
    pub background: Color,
    pub secondary: Color,
    pub highlight: Style,
    pub highlight_secondary: Style,
    pub border: Color,
    pub border_secondary: Color,
    pub text: Style,
    pub text_secondary: Style,
    pub text_inactive: Style,
    pub text_disabled: Style,
    pub text_muted: Style,
    pub text_bold: Style,
    pub text_italic: Style,
    pub text_underline: Style,
    pub text_blink: Style,
    pub text_dimmed: Style,

    // command bar
    pub commandbar_command: Style,
    pub commandbar_separator: Style,
    pub commandbar_description: Style,
}

impl Default for Theme {
    fn default() -> Self {
        Self {
            foreground: Color::White,
            background: Color::Rgb(26, 26, 26),
            secondary: Color::White,
            highlight: Style::default().fg(Color::White).bg(Color::Cyan),
            highlight_secondary: Style::default().fg(Color::Cyan),
            border: Color::White,
            border_secondary: Color::White,
            text: Style::default().fg(Color::White),
            text_secondary: Style::default().fg(Color::Yellow),
            text_inactive: Style::default().fg(Color::Gray),
            text_disabled: Style::default().fg(Color::Gray),
            text_muted: Style::default().fg(Color::Gray),
            text_bold: Style::default().add_modifier(Modifier::BOLD),
            text_italic: Style::default().add_modifier(Modifier::ITALIC),
            text_underline: Style::default().add_modifier(Modifier::UNDERLINED),
            text_blink: Style::default().add_modifier(Modifier::SLOW_BLINK),
            text_dimmed: Style::default().add_modifier(Modifier::DIM),

            commandbar_command: Style::default().fg(Color::LightBlue),
            commandbar_separator: Style::default().fg(Color::LightGreen),
            commandbar_description: Style::default().fg(Color::Red),
        }
    }
}

pub fn load(_path: &Option<PathBuf>) -> anyhow::Result<Theme> {
    Ok(Theme::default())
}
