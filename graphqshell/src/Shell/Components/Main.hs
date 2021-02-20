{-# LANGUAGE TemplateHaskell #-}

-- | The main component of the application.
-- Use this as the entry point to understand how the app works.
module Shell.Components.Main
  ( update,
    view,
    State,
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
import Shell.Components.Shared
import Shell.KeyMap
import Text.URI
  ( renderStr,
  )
import Utils

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
  EventChan ->
  State ->
  BrickEvent ComponentName Event ->
  EventM ComponentName (Next State)
update chan s (VtyEvent evt) = updateVTY (activeComponent s) chan s evt
update chan s (AppEvent evt@(KeyCommand _)) = do
  let newState = deactivateCommandBar s
  updateCommandBarEvent (activeComponent newState) chan newState evt
update chan s (AppEvent evt) = updateAppEvent (activeComponent s) chan s evt
update _ s _ = continue s

updateCommandBarEvent ::
  ComponentName ->
  EventChan ->
  State ->
  Event ->
  EventM ComponentName (Next State)
updateCommandBarEvent _ _ s (KeyCommand CmdQuit) = halt s
updateCommandBarEvent IntrospectorComponent chan s keyCommand = relayUpdate s (stIntrospectorRecord . crState) (Intro.update chan) (AppEvent keyCommand)
updateCommandBarEvent _ _ s _ = continue s

updateVTY ::
  ComponentName ->
  EventChan ->
  State ->
  V.Event ->
  EventM ComponentName (Next State)
updateVTY CommandBarComponent _ s (V.EvKey V.KEsc _) = continue (deactivateCommandBar s)
updateVTY CommandBarComponent chan s evt = relayUpdate s (stCommandBarRecord . crState) (CommandBar.update chan) (VtyEvent evt)
updateVTY _ _ s (V.EvKey (V.KChar ' ') []) = continue (activateComponent s CommandBarComponent)
updateVTY _ _ s (V.EvKey (V.KChar 'c') [V.MCtrl]) = halt s
updateVTY MainComponent _ s _ = continue s
updateVTY IntrospectorComponent chan s evt = relayUpdate s (stIntrospectorRecord . crState) (Intro.update chan) (VtyEvent evt)
updateVTY _ _ s _ = continue s

updateAppEvent ::
  ComponentName ->
  EventChan ->
  State ->
  Event ->
  EventM ComponentName (Next State)
updateAppEvent IntrospectorComponent chan s evt = relayUpdate s (stIntrospectorRecord . crState) (Intro.update chan) (AppEvent evt)
updateAppEvent _ _ s _ = continue s

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
