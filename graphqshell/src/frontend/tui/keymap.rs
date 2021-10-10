use tuirealm::event::{KeyCode, KeyEvent, KeyModifiers};
use tuirealm::Msg;

/// Globally available key map to match key events
pub const MSG_KEY_ESC: Msg = Msg::OnKey(KeyEvent {
    code: KeyCode::Esc,
    modifiers: KeyModifiers::NONE,
});

#[allow(dead_code)]
pub const MSG_KEY_TAB: Msg = Msg::OnKey(KeyEvent {
    code: KeyCode::Tab,
    modifiers: KeyModifiers::NONE,
});
