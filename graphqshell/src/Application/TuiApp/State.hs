module Application.TuiApp.State where

import qualified Application.TuiApp.Components.CommandBar.State as CommandBar
import qualified Application.TuiApp.Components.Main.State as Main
import Application.TuiApp.Shared (ComponentName, Event)
import Infrastructure.TuiEngine.Keymap (KeyMap)

data State = State
  { _stKeyMap :: KeyMap Event,
    _stComponentStack :: ![ComponentName],
    _componentStateMain :: !Main.State,
    _componentStateCommandBar :: !CommandBar.State
  }
