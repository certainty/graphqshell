module Application.TuiApp.Components.CommandBar.Component where

import Application.TuiApp.Components.CommandBar.Behavior
import Application.TuiApp.Components.CommandBar.State (State)
import Application.TuiApp.Shared (Action, ComponentName (CommandBar), Event)
import Control.Exception.Safe (MonadThrow)
import Infrastructure.TuiEngine.Components
import Infrastructure.TuiEngine.Keymap (KeyMap)
import Relude hiding (State, state)

type ComponentType m = Component State Action Event ComponentName m

makeComponent :: (MonadThrow m) => KeyMap Event -> Component State Action Event ComponentName m
makeComponent keyMap =
  Component
    { componentName = CommandBar,
      componentInitial = initial keyMap,
      componentUpdate = update,
      componentView = Just view
    }
