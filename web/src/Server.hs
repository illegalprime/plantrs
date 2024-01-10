module Server (
  app,
) where

import Api
import Commands (Commander, runCommand)
import Config (Topics (Topics, responseTopic))
import Control.Concurrent (Chan)
import Network.MQTT.Client (MQTTClient, QoS (QoS1), SubOptions (_subQoS), subOptions, subscribe)
import Network.Wai.Middleware.Cors (simpleCors)
import Paths_plantrs (getDataDir)
import Servant

waterHandler :: Commander -> Text -> Maybe Word32 -> Handler Text
waterHandler commander plant secs = do
  liftIO $ commander plant $ Drive $ fromMaybe 0 secs

addHandler :: Commander -> Text -> AddReq -> Handler Text
addHandler commander plant AddReq {a, b} = do
  liftIO $ commander plant $ Add a b

discoverHandler :: MVar (Set Text) -> Handler [Client]
discoverHandler = (map toClient . toList <$>) . readMVar
  where
    toClient c = Client {id = c, name = getName c}

server :: FilePath -> Commander -> MVar (Set Text) -> Server AppApi
server static cmd clients =
  waterHandler cmd
    :<|> addHandler cmd
    :<|> discoverHandler clients
    :<|> serveDirectoryFileServer static

app :: Topics -> MQTTClient -> Chan LByteString -> MVar (Set Text) -> IO Application
app topics@Topics {responseTopic} mc msgs clients = do
  -- directory to serve static files from
  static <- getDataDir
  -- init command handler
  let commander = runCommand topics mc msgs
  -- subscribe to command response topic
  print =<< subscribe mc [(topic, options)] []
  -- build app description
  pure $ simpleCors $ serve appApi $ server static commander clients
  where
    topic = fromString . toString $ responseTopic
    options = subOptions {_subQoS = QoS1}

-- TODO: add user-specified name
getName :: Text -> Text
getName "lime-tree" = "Lime Tree"
getName _ = "Unknown"
