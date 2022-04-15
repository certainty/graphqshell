{-# LANGUAGE TemplateHaskell #-}

module GQShell.Application.TUI.Activities.Introspector where

import Brick (Widget)
import Brick.Widgets.Core (txt)
import GQShell.Application.TUI.Shared
import Hubble.Program (UpdateM, cont, mkModel)
import Lens.Micro.Platform hiding (view)
import qualified Lens.Micro.Platform as Lens
import Relude

data IntrospectorState = IntrospectorState
  { _isFocus :: Focus
  }

makeLenses ''IntrospectorState

type IntrospectorModel = Model' IntrospectorState

instance Focusable IntrospectorState where
  focusL = isFocus

newModel :: Focus -> IntrospectorModel
newModel f = mkModel (IntrospectorState f) mempty update view

view :: IntrospectorState -> Widget ()
view _ = txt "introspector"

update :: IntrospectorState -> Message' -> UpdateM (IntrospectorState, [Command'])
update s _ = cont s
