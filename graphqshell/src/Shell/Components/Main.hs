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

data ComponentRecord s = ComponentRecord
  { _crName :: ComponentName,
    _crState :: s,
    _crKeyMap :: Maybe (KeyMap Command)
  }

makeLenses ''ComponentRecord

-- | The application state holds global data and component specific data.
-- The application will delegate updates to the currently active component automatically.
data State = State
  { -- | The 'Schema' of the currently connected GraphQL API
    _stSchema :: Schema,
    -- | settings for the 'API' client
    _stApiSettings :: API.ApiSettings,
    -- | the registered components
    _stComponentStack :: ![ComponentName],
    _stCommandBarRecord :: ComponentRecord (CommandBar.State Command),
    _stIntrospectorRecord :: ComponentRecord Intro.State
  }

makeLenses ''State

-- Application KeyMap
mainKeyMapConfig :: KeyMapConfiguration Command
mainKeyMapConfig = cmd 'q' "Quit" CmdQuit

{-
  ___       _ _
 |_ _|_ __ (_) |_
  | || '_ \| | __|
  | || | | | | |_
 |___|_| |_|_|\__|

-}

initialState :: (MonadThrow m) => API.ApiSettings -> Schema -> m State
initialState settings schema = do
  mainKeyMap <- fromConfiguration mainKeyMapConfig
  pure $
    State
      schema
      settings
      componentStack
      (commandBarRecord mainKeyMap)
      (introspectorRecord Nothing)
  where
    componentStack = [IntrospectorComponent, MainComponent]
    commandBarRecord keyMap = ComponentRecord CommandBarComponent (CommandBar.initialState keyMap) Nothing
    introspectorRecord keyMap = ComponentRecord IntrospectorComponent (Intro.initialState schema (Object (query schema))) keyMap

activeComponent :: State -> ComponentName
activeComponent s = case s ^. stComponentStack of
  [] -> MainComponent
  (active : _) -> active

activateComponent :: State -> ComponentName -> State
activateComponent s component = activateComponentKeyMap component (set stComponentStack (component : stack) s)
  where
    stack = s ^. stComponentStack

deactivateCurrentComponent :: State -> State
deactivateCurrentComponent s = activateComponentKeyMap (activeComponent stateWithUpdatedStack) stateWithUpdatedStack
  where
    stateWithUpdatedStack = set stComponentStack (drop 1 $ s ^. stComponentStack) s

activateComponentKeyMap :: ComponentName -> State -> State
activateComponentKeyMap MainComponent s = s
activateComponentKeyMap IntrospectorComponent s = s
activateComponentKeyMap CommandBarComponent s = s

deactivateCommandBar :: State -> State
deactivateCommandBar state = deactivateCurrentComponent $ set cmdState updatedCommandBarState state
  where
    cmdState = stCommandBarRecord . crState
    updatedCommandBarState = CommandBar.resetState (state ^. stCommandBarRecord . crState)

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
update s (AppEvent (CommandBarEvent (CommandBar.CommandSelected evt))) = updateCommandBarEvent (activeComponent s) s evt
update s (AppEvent evt) = updateAppEvent (activeComponent s) s evt
update s _ = keepGoing s

updateCommandBarEvent ::
  p ->
  s ->
  Command ->
  EventM n (Continuation e s)
updateCommandBarEvent _ s CmdQuit = stopIt s
updateCommandBarEvent _ s _ = keepGoing s

updateVTY ::
  ComponentName ->
  State ->
  V.Event ->
  EventM ComponentName (Continuation Event State)
updateVTY CommandBarComponent s (V.EvKey V.KEsc _) = keepGoing (deactivateCommandBar s)
updateVTY CommandBarComponent s evt = updateComponent s (stCommandBarRecord . crState) CommandBarEvent CommandBar.update (VtyEvent evt)
updateVTY _ s (V.EvKey (V.KChar ' ') []) = keepGoing (activateComponent s CommandBarComponent)
updateVTY _ s (V.EvKey (V.KChar 'c') [V.MCtrl]) = stopIt s
updateVTY MainComponent s _ = keepGoing s
updateVTY IntrospectorComponent s evt = updateComponent s (stIntrospectorRecord . crState) IntrospectorEvent Intro.update (VtyEvent evt)

updateAppEvent ::
  ComponentName ->
  State ->
  Event ->
  EventM ComponentName (Continuation Event State)
updateAppEvent IntrospectorComponent s (IntrospectorEvent evt) = updateComponent s (stIntrospectorRecord . crState) IntrospectorEvent Intro.update (AppEvent evt)
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
      <=> Intro.view (state ^. stIntrospectorRecord . crState)
      <=> hBorder
      <=> statusLine state

-- TODO: extract into component
tabLine :: Widget ComponentName
tabLine = hBox [padRight Max $ padLeft (Pad 1) $ txt "[ Introspector ] | [ Query ]"]

-- TODO: extract into component
statusLine :: State -> Widget ComponentName
statusLine state = case (activeComponent state) of
  CommandBarComponent -> CommandBar.view (state ^. stCommandBarRecord . crState) <=> hBorder <=> status
  _ -> status
  where
    status = hBox [padRight Max $ padLeft (Pad 1) $ txt "Status bar will be used for updates soon"]
