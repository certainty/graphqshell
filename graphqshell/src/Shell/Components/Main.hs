{-# LANGUAGE TemplateHaskell #-}

-- | The main component of the application.
-- Use this as the entry point to understand how the app works.
module Shell.Components.Main
  ( update,
    view,
    State,
    Event (..),
    initialState,
    attributes,
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
import Graphics.Vty.Attributes
  ( Attr,
  )
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
  | CommandBarEvent (CommandBar.Event CommandBarCommand)
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
    _crKeyMap :: Maybe (KeyMap CommandBarCommand)
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
    _stKeyMap :: KeyMap CommandBarCommand,
    _stComponentStack :: ![ComponentName],
    _stCommandBarRecord :: ComponentRecord (CommandBar.State CommandBarCommand),
    _stIntrospectorRecord :: ComponentRecord Intro.State
  }

makeLenses ''State

{-
  _  __
 | |/ /___ _  _ _ __  __ _ _ __
 | ' </ -_) || | '  \/ _` | '_ \
 |_|\_\___|\_, |_|_|_\__,_| .__/
           |__/           |_|

-}
-- Application KeyMap
mainKeyMapConfig :: KeyMapConfiguration CommandBarCommand
mainKeyMapConfig = cmd 'q' "quit" CmdQuit

{-
     _   _   _        _ _           _
    / \ | |_| |_ _ __(_) |__  _   _| |_ ___  ___
   / _ \| __| __| '__| | '_ \| | | | __/ _ \/ __|
  / ___ \ |_| |_| |  | | |_) | |_| | ||  __/\__ \
 /_/   \_\__|\__|_|  |_|_.__/ \__,_|\__\___||___/

-}

attrStatusLine :: AttrName
attrStatusLine = "main" <> "statusLine"

attrTopBar :: AttrName
attrTopBar = "main" <> "topBar"

attributes :: [(AttrName, Attr)]
attributes = [(attrStatusLine, V.defAttr), (attrTopBar, V.defAttr)]

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
  introspectorKeyMap <- sequence (fromConfiguration <$> Intro.keyMapConfig)
  pure $
    activateComponent
      ( State
          schema
          settings
          mainKeyMap
          componentStack
          (commandBarRecord mainKeyMap)
          (introspectorRecord introspectorKeyMap)
      )
      IntrospectorComponent
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
activateComponentKeyMap IntrospectorComponent s = case s ^. stIntrospectorRecord . crKeyMap of
  (Just keyMap) -> activateComponentKeyMap' s keyMap "+introspector"
  Nothing -> s
activateComponentKeyMap CommandBarComponent s = s

activateComponentKeyMap' :: State -> KeyMap CommandBarCommand -> Text -> State
activateComponentKeyMap' s componentKeyMap caption =
  set (stCommandBarRecord . crState) updatedCommandBarState (set stKeyMap updatedKeyMap s)
  where
    updatedCommandBarState = CommandBar.initialState updatedKeyMap
    updatedKeyMap = insertBinding (s ^. stKeyMap) ' ' (Group caption componentKeyMap)

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
update s (AppEvent (CommandBarEvent (CommandBar.CommandSelected evt))) = fmap deactivateCommandBar <$> updateCommandBarEvent (activeComponent s) s evt
update s (AppEvent evt) = updateAppEvent (activeComponent s) s evt
update s _ = keepGoing s

updateCommandBarEvent ::
  ComponentName ->
  s ->
  CommandBarCommand ->
  EventM n (Continuation e s)
updateCommandBarEvent _ s CmdQuit = stopIt s
updateCommandBarEvent IntrospectorComponent s cmd = keepGoing s -- TODO: implement this so that the component is updated. But first Continuation needs to become a monad

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
topBar state = withAttr attrTopBar $ hBox [padRight Max $ txt (toText url)]
  where
    url = renderStr . API.apiURI $ state ^. stApiSettings

mainViewPort :: State -> Widget ComponentName
mainViewPort state =
  border $
    topBar state
      <=> hBorder
      <=> Intro.view (state ^. stIntrospectorRecord . crState)
      <=> hBorder
      <=> statusLine state

statusLine :: State -> Widget ComponentName
statusLine state = case (activeComponent state) of
  CommandBarComponent -> CommandBar.view (state ^. stCommandBarRecord . crState) <=> hBorder <=> status (statusLineComponentName CommandBarComponent)
  name -> status (statusLineComponentName name)
  where
    status componentName = withAttr attrStatusLine $ hBox [padRight Max $ padLeft (Pad 1) $ txt componentName]

statusLineComponentName :: ComponentName -> Text
statusLineComponentName CommandBarComponent = "Menu"
statusLineComponentName IntrospectorComponent = "Introspector"
statusLineComponentName MainComponent = "Main"
