{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Infrastructure.TuiEngine.Components
  ( Continuation (..),
    Component (..),
    componentInitial,
    componentUpdate,
    componentView,
    componentName,
    TheComponent (..),
  )
where

import Brick (Widget)
import Control.Monad.Catch.Pure (MonadThrow)
import Infrastructure.TuiEngine.Events (Event)
import Lens.Micro.Platform (makeLenses)
import Relude

data Continuation state action event
  = Notify state event
  | Perform state action
  | PerformAndNotify state action event
  | Continue state
  | Quit state

data Component state action event name m = Component
  { _componentName :: name,
    _componentInitial :: (MonadThrow m) => m (Continuation state action event),
    _componentUpdate :: (MonadThrow m) => state -> Event event -> m (Continuation state action event),
    -- | components might be renderable but are not required to be
    _componentView :: Maybe (state -> [Widget name])
  }

makeLenses ''Component

class TheComponent state action event name | state -> action event name where
  initialx :: state
  namex :: state -> name
  updatex :: (MonadThrow m) => state -> Event event -> m (Continuation state action event)
  viewx :: Maybe (state -> Widget name)
