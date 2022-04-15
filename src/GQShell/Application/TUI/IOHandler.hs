module GQShell.Application.TUI.IOHandler where

import Control.Exception.Safe (catchAny)
import GQShell.Application.TUI.Shared
import qualified GQShell.Core.GraphQL.API as API
import qualified GQShell.Infrastructure.GraphQL.IOApi as API
import Hubble.CommandHandler (CommandHandler, sendMessage)
import qualified Hubble.CommandHandler as CommandHandler
import Relude

catchingErrors :: IO a -> IO (Maybe a)
catchingErrors action = (Just <$> action) `catchAny` (\_ -> pure Nothing)

commandHandler :: CommandHandler AppCommand AppMessage
commandHandler (EmitMessages msgs) = do
  traverse_ sendMessage msgs
commandHandler (ConnectEndpoint cfg) = do
  CommandHandler.logInfo "[IO] changes endpoint"
  sendMessage (EndpointChange (Connecting cfg))
  let settings = API.mkApiSettings cfg
  schemaResult <- liftIO $ catchingErrors (API.runApiIO settings API.introspect)
  case schemaResult of
    Just schema -> sendMessage (EndpointChange (Connected cfg schema))
    Nothing -> sendMessage (EndpointError "Could not connect")
