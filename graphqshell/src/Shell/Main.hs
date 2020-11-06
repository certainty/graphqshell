{-# LANGUAGE OverloadedStrings #-}

module Shell.Main(runShell) where
import Control.Monad (void)
import Brick
  ( App(..), BrickEvent(..), EventM, Next, Widget
  , neverShowCursor
  , continue
  , padRight, Padding(..)
  , attrMap
  , (<+>)
  , str
  , defaultMain
  )
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V


data Name = SchemaView deriving (Eq, Ord, Show)

-- Welcome the app-state
data GQShellState = GQShellState {
  _url :: String
} deriving (Eq, Show)

-- Application Events
data GQShellEvent =
    SchemaEvent |
    Tick
  deriving (Eq, Ord, Show)


runShell :: String -> IO ()
runShell url = void $ defaultMain application state
  where
    state = GQShellState url

application :: App GQShellState GQShellEvent ()
application = App {   appDraw = draw
                    , appChooseCursor = neverShowCursor
                    , appHandleEvent = handleEvent
                    , appAttrMap = const $ attrMap V.defAttr []
                    , appStartEvent = return
                  }

handleEvent :: GQShellState -> BrickEvent n GQShellEvent -> EventM n (Next GQShellState)
handleEvent s (AppEvent _) = continue s

draw :: GQShellState -> [Widget n]
draw _ = [str "Hello World"]



