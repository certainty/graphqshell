{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Application.TuiApp.Components.Main.State where

import qualified Application.TuiApp.Components.CommandBar.Component as CommandBar
import Application.TuiApp.Shared (ComponentName, Event)
import Infrastructure.TuiEngine.Keymap (KeyMap)
import Optics.TH
import Relude (IO)

data State = State
  { keyMap :: KeyMap Event,
    componentCommandBar :: CommandBar.ComponentType IO,
    componentStack :: ![ComponentName]
  }

makeFieldLabelsWith noPrefixFieldLabels ''State
