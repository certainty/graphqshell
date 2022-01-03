{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Application.TuiApp.Components.CommandBar.State where

import Application.TuiApp.Shared (Event)
import Infrastructure.TuiEngine.Keymap (KeyMap)
import Lens.Micro.Platform (makeLenses)

data State = State
  { _stRootKeyMap :: KeyMap Event,
    _stActiveKeyMap :: KeyMap Event
  }

makeLenses ''State
