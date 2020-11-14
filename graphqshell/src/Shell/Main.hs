{-# LANGUAGE OverloadedStrings #-}

module Shell.Main(runShell) where
import Relude
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V
import qualified Shell.Components.Introspection as Intro


data Name = SchemaView deriving (Eq, Ord, Show)

-- Welcome the app-state
data GQShellState = GQShellState {
  _url :: String
} deriving (Eq, Show)

-- Application Events
data GQShellEvent = SchemaEvent | Tick
  deriving (Eq, Ord, Show)

runShell :: String -> IO ()
runShell url = void $ defaultMain makeApplication state
  where
    state = GQShellState url

makeApplication :: App GQShellState GQShellEvent ()
makeApplication = App
  {
     appDraw         = draw
   , appChooseCursor = neverShowCursor
   , appHandleEvent  = handleEvent
   , appAttrMap      = const $ attrMap V.defAttr []
   , appStartEvent   = pure
 }

handleEvent :: GQShellState -> BrickEvent n GQShellEvent -> EventM n (Next GQShellState)
handleEvent s (AppEvent _) = continue s

draw :: GQShellState -> [Widget ()]
draw st = [ui (_url st)]

topBar :: String -> Widget n 
topBar url = hBox [ padRight Max $ str $ "connected to " ++ url ]

statusLine :: Widget ()
statusLine = hBox [ padRight Max $ str "Status"]

mainViewPort :: String -> Widget ()
mainViewPort url = border $
  (topBar url)
  <=>
  hBorder
  <=>
  Intro.view
  <=>
  hBorder
  <=>
  statusLine

ui :: String -> Widget ()
ui url = withBorderStyle unicodeRounded $ joinBorders  $
  mainViewPort url
