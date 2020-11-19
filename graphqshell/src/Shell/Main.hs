{-# LANGUAGE OverloadedStrings #-}

module Shell.Main (runShell) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified GraphQL.API as API
import GraphQL.Introspection.Schema.Types (Schema)
import qualified Graphics.Vty as V
import Relude
import qualified Shell.Components.Introspection as Intro
import Text.URI (renderStr)

data Name = SchemaView deriving (Eq, Ord, Show)

-- Welcome the app-state
data GQShellState = GQShellState
  { _schema :: Schema
  }
  deriving (Eq, Show)

-- Application Events
data GQShellEvent = SchemaEvent | Tick
  deriving (Eq, Ord, Show)

runShell :: String -> IO ()
runShell url = do
  --  api <- (API.mkAPI (toText url))
  -- schema <- API.introspect api
  void $ defaultMain makeApplication (GQShellState schema api)

makeApplication :: App GQShellState GQShellEvent ()
makeApplication =
  App
    { appDraw = draw,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appAttrMap = const $ attrMap V.defAttr [],
      appStartEvent = pure
    }

handleEvent :: GQShellState -> BrickEvent n GQShellEvent -> EventM n (Next GQShellState)
handleEvent s (AppEvent _) = continue s

draw :: GQShellState -> [Widget ()]
draw st = [ui uriStr]
  where
    uriStr = renderStr . API.endpointURI . _api $ st

topBar :: String -> Widget n
topBar url = hBox [padRight Max $ str $ "connected to " ++ url]

statusLine :: Widget ()
statusLine = hBox [padRight Max $ str "Status"]

mainViewPort :: String -> Widget ()
mainViewPort url =
  border $
    (topBar url)
      <=> hBorder
      <=> Intro.view
      <=> hBorder
      <=> statusLine

ui :: String -> Widget ()
ui url =
  withBorderStyle unicodeRounded $
    joinBorders $
      mainViewPort url
