module Application.TuiApp.Components.Main.Component where

import Application.TuiApp.Components.Main.Behavior
import Application.TuiApp.Components.Main.State (State)
import Application.TuiApp.Shared (Action, ComponentName (Main), Event)
import Control.Exception.Safe (MonadThrow)
import Infrastructure.TuiEngine.Components
import Relude hiding (State)

type ComponentType m = Component State Action Event ComponentName m

makeComponent :: (MonadThrow m) => (Component State Action Event ComponentName m)
makeComponent =
  Component
    { componentName = Main,
      componentInitial = initial,
      componentUpdate = update,
      componentView = Just view
    }
