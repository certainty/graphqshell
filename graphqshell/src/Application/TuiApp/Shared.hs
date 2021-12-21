module Application.TuiApp.Shared where

import Relude

data ComponentName = Main | CommandBar deriving (Show, Eq, Ord)

data Action

data CommandBarEvent
  = CmdBarQuit
  | CmdBarNoop
  | CmdBarIntrospectorGotoQuery
  deriving (Eq, Show, Ord)

data Event = CmdQuit | EventFromCommandBar CommandBarEvent deriving (Show, Eq, Ord)
