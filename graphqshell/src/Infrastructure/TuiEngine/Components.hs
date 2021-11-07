{-# LANGUAGE TemplateHaskell #-}

module Infrastructure.TuiEngine.Components
  ( Continuation (..),
    Component (..),
    componentAttributes,
    componentInitial,
    componentUpdate,
    componentView,
    componentName,
    componentKeymap,
  )
where

import Brick (AttrMap, Widget)
import Infrastructure.TuiEngine.Events (Event)
import Infrastructure.TuiEngine.Keymap
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
    _componentAttributes :: state -> AttrMap,
    _componentInitial :: Continuation state action event,
    _componentUpdate :: state -> Event event -> Continuation state action event,
    -- | components might be renderable
    _componentView :: Maybe (state -> [Widget name]),
    -- | components might have context sensitive keymap
    _componentKeymap :: Maybe (state -> KeyMapConfiguration event)
  }

makeLenses ''Component
