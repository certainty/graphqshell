{-# LANGUAGE TemplateHaskell #-}

module Infrastructure.TuiEngine.Components
  ( Continuation (..),
    Component (..),
    componentInitial,
    componentUpdate,
    componentView,
    componentName,
  )
where

import Brick (Widget)
import Infrastructure.TuiEngine.Events (Event)
import Lens.Micro.Platform (makeLenses)
import Relude

data Continuation state action event
  = Notify state event
  | Perform state action
  | PerformAndNotify state action event
  | Continue state
  | Quit state

data Component state action event name = Component
  { _componentName :: name,
    _componentInitial :: Continuation state action event,
    _componentUpdate :: state -> Event event -> Continuation state action event,
    -- | components might be renderable
    _componentView :: Maybe (state -> [Widget name])
  }

makeLenses ''Component
