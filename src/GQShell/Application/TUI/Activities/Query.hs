{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.TUI.Activities.Query where

import Brick (Widget)
import Brick.Widgets.Core (txt)
import GQShell.Application.TUI.Shared
import Hubble.Program (UpdateM, cont, mkModel)
import Lens.Micro.Platform hiding (view)
import Relude

data QueryState = QueryState
  { _qsFocus :: Focus
  }

makeLenses ''QueryState

type QueryModel = Model' QueryState

instance Focusable QueryState where
  focusL = qsFocus

newModel :: Focus -> QueryModel
newModel f = mkModel (QueryState f) mempty update view

view :: QueryState -> Widget ()
view _ = txt "introspector"

update :: QueryState -> Message' -> UpdateM (QueryState, [Command'])
update s _ = cont s
