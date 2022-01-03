{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Application.TuiApp.Components.Main.State where

import qualified Application.TuiApp.Components.CommandBar.Component as CommandBar
import Application.TuiApp.Shared (ComponentName, Event)
import Infrastructure.TuiEngine.Keymap (KeyMap)
import Lens.Micro.Platform (makeLenses)
import Relude (IO)

data State = State
  { _stKeyMap :: KeyMap Event,
    _componentCommandBar :: CommandBar.ComponentType IO,
    _stComponentStack :: ![ComponentName]
  }

makeLenses ''State
