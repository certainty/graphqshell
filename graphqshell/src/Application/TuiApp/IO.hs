module Application.TuiApp.IO (ioHandler) where

import Application.TuiApp.Shared (Action, Event)
import Infrastructure.TuiEngine (IOHandler)
import Relude

ioHandler :: IOHandler Event Action
ioHandler _ = pure Nothing
