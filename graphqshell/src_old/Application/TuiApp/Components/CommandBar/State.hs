{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Application.TuiApp.Components.CommandBar.State where

import Application.TuiApp.Shared (Event)
import Infrastructure.TuiEngine.Keymap (KeyMap)
import Optics.TH

data State = State
  { rootKeyMap :: KeyMap Event,
    activeKeyMap :: KeyMap Event
  }

makeFieldLabelsWith noPrefixFieldLabels ''State
