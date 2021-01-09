{-# LANGUAGE TemplateHaskell #-}

-- | The main component of the application.
-- Use this as the entry point to understand how the app works.
module Shell.Components.Main
  ( update,
    view,
    State,
    Event (..),
    initialState,
  )
where

import Brick
import qualified Brick.Focus as Focus
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Exception.Safe (MonadThrow)
import qualified GraphQL.API as API
import GraphQL.Introspection.Schema
  ( GraphQLType
      ( Object
      ),
    Schema,
    query,
  )
import qualified Graphics.Vty as V
import Lens.Micro.Platform
  ( makeLenses,
    set,
    (^.),
  )
import Relude hiding
  ( State,
    state,
  )
import qualified Shell.Components.CommandBar as CommandBar
import qualified Shell.Components.Introspector as Intro
import Shell.Components.Types
import Shell.Continuation
import Shell.KeyMap
import Text.URI
  ( renderStr,
  )

data Event
  = IntrospectorEvent Intro.Event
  | CommandBarEvent (CommandBar.Event Command)
  | Tick
  deriving (Eq, Show)

{-
  ____  _        _
 / ___|| |_ __ _| |_ ___
 \___ \| __/ _` | __/ _ \
  ___) | || (_| | ||  __/
 |____/ \__\__,_|\__\___|

-}

-- | The application state holds global data and component specific data.
-- The application will delegate updates to the currently active component automatically.
data State = State
  { -- | The 'Schema' of the currently connected GraphQL API
    _stSchema :: Schema,
    -- | settings for the 'API' client
    _stApiSettings :: API.ApiSettings,
    -- | Manage which component has the focus
    _stFocus :: !(Focus.FocusRing ComponentName),
    -- |
    _stComponentStack :: ![ComponentName],
    -- | State for the introspector component
    _stIntrospectorState :: Intro.State,
    -- | State for the introspector component
    _stGlobalCommandBarState :: CommandBar.State Command,
    -- |
    _stContextCommandBarState :: CommandBar.State Command
  }

makeLenses ''State

-- KeyMap
globalKeyMapConfig :: KeyMapConfiguration Command
globalKeyMapConfig = cmd 'q' "Quit" (Global CmdQuit)

contextKeyMapConfig :: KeyMapConfiguration Command
contextKeyMapConfig =
  cmd 'a' "Test" (Context Noop)
    <> cmd 'b' "Test 2" (Context Noop)
    <> (sub 'c' "+Group" (cmd 's' "Foobar" (Context Noop)))

{-
  ___       _ _
 |_ _|_ __ (_) |_
  | || '_ \| | __|
  | || | | | | |_
 |___|_| |_|_|\__|

-}

initialState :: (MonadThrow m) => API.ApiSettings -> Schema -> m State
initialState settings schema = do
  globalKeyMap <- compile globalKeyMapConfig
  contextKeyMap <- compile contextKeyMapConfig
  pure $
    State
      schema
      settings
      (Focus.focusRing components)
      [IntrospectorComponent, MainComponent]
      (Intro.initialState schema (Object (query schema)))
      (CommandBar.initialState globalKeyMap)
      (CommandBar.initialState contextKeyMap)
  where
    components = [MainComponent, GlobalCommandBarComponent, ContextCommandBarComponent, IntrospectorComponent]

activeComponent :: State -> ComponentName
activeComponent s = case s ^. stComponentStack of
  [] -> MainComponent
  (active : _) -> active

activateComponent :: State -> ComponentName -> State
activateComponent s component = set stComponentStack (component : stack) s
  where
    stack = s ^. stComponentStack

deactivateCurrentComponent :: State -> State
deactivateCurrentComponent s = set stComponentStack (drop 1 $ s ^. stComponentStack) s

deactivateCommandBars :: State -> State
deactivateCommandBars state =
  deactivateCurrentComponent $
    state
      { _stGlobalCommandBarState = (CommandBar.resetState (state ^. stGlobalCommandBarState)),
        _stContextCommandBarState = (CommandBar.resetState (state ^. stContextCommandBarState))
      }

{-
  _   _           _       _
 | | | |_ __   __| | __ _| |_ ___
 | | | | '_ \ / _` |/ _` | __/ _ \
 | |_| | |_) | (_| | (_| | ||  __/
  \___/| .__/ \__,_|\__,_|\__\___|
       |_|

-}

update ::
  State ->
  BrickEvent ComponentName Event ->
  EventM ComponentName (Continuation Event State)
update s (VtyEvent evt) = updateVTY (activeComponent s) s evt
update s (AppEvent (CommandBarEvent (CommandBar.CommandSelected (Global evt)))) = updateGlobalCommandBarEvent (activeComponent s) s evt
update s (AppEvent (CommandBarEvent (CommandBar.CommandSelected (Context evt)))) = updateContextCommandBarEvent (activeComponent s) s evt
update s (AppEvent evt) = updateAppEvent (activeComponent s) s evt
update s _ = keepGoing s

updateGlobalCommandBarEvent _ s CmdQuit = stopIt s
updateGlobalCommandBarEvent _ s _ = keepGoing s

updateContextCommandBarEvent _ s _ = keepGoing s

updateVTY ::
  ComponentName ->
  State ->
  V.Event ->
  EventM ComponentName (Continuation Event State)
updateVTY GlobalCommandBarComponent s (V.EvKey V.KEsc _) = keepGoing (deactivateCommandBars s)
updateVTY ContextCommandBarComponent s (V.EvKey V.KEsc _) = keepGoing (deactivateCommandBars s)
updateVTY ContextCommandBarComponent s evt = updateComponent s stContextCommandBarState CommandBarEvent CommandBar.update (VtyEvent evt)
updateVTY GlobalCommandBarComponent s evt = updateComponent s stGlobalCommandBarState CommandBarEvent CommandBar.update (VtyEvent evt)
updateVTY _ s (V.EvKey (V.KChar ' ') []) = keepGoing (activateComponent s ContextCommandBarComponent)
updateVTY _ s (V.EvKey (V.KChar 'c') [V.MCtrl]) = keepGoing (activateComponent s GlobalCommandBarComponent)
updateVTY MainComponent s _ = keepGoing s
updateVTY IntrospectorComponent s evt = updateComponent s stIntrospectorState IntrospectorEvent Intro.update (VtyEvent evt)

updateAppEvent ::
  ComponentName ->
  State ->
  Event ->
  EventM ComponentName (Continuation Event State)
updateAppEvent IntrospectorComponent s (IntrospectorEvent evt) = updateComponent s stIntrospectorState IntrospectorEvent Intro.update (AppEvent evt)
updateAppEvent _ s _ = keepGoing s

{-
 __     ___
 \ \   / (_) _____      __
  \ \ / /| |/ _ \ \ /\ / /
   \ V / | |  __/\ V  V /
    \_/  |_|\___| \_/\_/

-}

view :: State -> [Widget ComponentName]
view state = [mainWidget state]

mainWidget :: State -> Widget ComponentName
mainWidget state = withBorderStyle unicodeRounded $ joinBorders $ mainViewPort state

-- TODO: extract into component
topBar :: State -> Widget ComponentName
topBar state = hBox [padRight Max $ padLeft (Pad 1) $ txt (toText url)]
  where
    url = renderStr . API.apiURI $ state ^. stApiSettings

mainViewPort :: State -> Widget ComponentName
mainViewPort state =
  border $
    topBar state
      <=> hBorder
      <=> tabLine
      <=> hBorder
      <=> Intro.view (state ^. stIntrospectorState)
      <=> hBorder
      <=> statusLine state

-- TODO: extract into component
tabLine :: Widget ComponentName
tabLine = hBox [padRight Max $ padLeft (Pad 1) $ txt "[ Introspector ] | [ Query ]"]

-- TODO: extract into component
statusLine :: State -> Widget ComponentName
statusLine state = case (activeComponent state) of
  GlobalCommandBarComponent -> CommandBar.view (state ^. stGlobalCommandBarState) <=> hBorder <=> status
  ContextCommandBarComponent -> CommandBar.view (state ^. stContextCommandBarState) <=> hBorder <=> status
  _ -> status
  where
    status = hBox [padRight Max $ padLeft (Pad 1) $ txt "Status bar will be used for updates soon"]
