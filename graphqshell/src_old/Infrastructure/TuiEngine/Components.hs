{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.TuiEngine.Components where

import Brick (Widget)
import Control.Monad.Catch.Pure (MonadThrow)
import Infrastructure.TuiEngine.Events (Event)
import Relude

data Continuation state action event
  = Notify state event
  | Perform state action
  | PerformAndNotify state action event
  | Continue state
  | Quit state

data Component state action event name m = Component
  { componentName :: name,
    componentInitial :: (MonadThrow m) => m (Continuation state action event),
    componentUpdate :: (MonadThrow m) => state -> Event event -> m (Continuation state action event),
    -- | components might be renderable but are not required to be
    componentView :: Maybe (state -> [Widget name])
  }
