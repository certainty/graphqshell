module Hubble.Hubbles.Log where

import Brick (Widget, hBox, hLimit, txt, vBox, viewport, withAttr)
import Brick.AttrMap
import Brick.Types (ViewportType (Vertical))
import Brick.Widgets.Border (border, hBorder)
import Brick.Widgets.Core (vLimit)
import Hubble.Internal.Types (Message, Model)
import Hubble.Program (LogMessage (LogMessage), LogSeverity (..), UpdateM, cont, logMessages, mkModel)
import Relude

data LogState = LogState
  { _height :: Int,
    _minSeverity :: LogSeverity,
    _logMessages :: [LogMessage]
  }

type LogModel cmd msg = Model LogState cmd msg

newModel :: Int -> LogSeverity -> LogModel cmd msg
newModel height minSeverity = mkModel (LogState height minSeverity mempty) mempty logUpdate logView

logUpdate :: LogState -> Message msg -> UpdateM (LogState, [cmd])
logUpdate logState _ = do
  let currentMessages = _logMessages logState
  newMessages <- matchingLogMessges <$> logMessages
  cont (logState {_logMessages = newMessages ++ currentMessages})
  where
    matchingLogMessges = filter (\(LogMessage severity _) -> severity >= _minSeverity logState)

logView :: LogState -> Widget ()
logView logState =
  border $
    vLimit
      (_height logState)
      $ vBox
        [ txt $ "Logs with  severity >= " <> show (_minSeverity logState),
          hBorder,
          viewport () Vertical $ vBox logLines
        ]
  where
    logLines = fmap toLogLine (_logMessages logState)
    toLogLine (LogMessage Info msg) = withAttr logInfo $ hBox [lead "Info:  ", txt msg]
    toLogLine (LogMessage Warning msg) = withAttr logWarning $ hBox [lead "Warn:  ", txt msg]
    toLogLine (LogMessage Error msg) = withAttr logError $ hBox [lead "Error: ", txt msg]
    lead m = withAttr logLevel $ txt m

logLevel :: AttrName
logLevel = attrName "log" <> attrName "level"

logInfo :: AttrName
logInfo = attrName "log" <> attrName "info"

logWarning :: AttrName
logWarning = attrName "log" <> attrName "warning"

logError :: AttrName
logError = attrName "log" <> attrName "error"
