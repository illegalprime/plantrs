module Server (
  app,
) where

import Api
import Commands (Commander, runCommand)
import Config (Topics (Topics, responseTopic))
import Control.Concurrent (Chan)
import Database (findPlant, labelPlant, listPlants)
import Database.Persist.Sql (ConnectionPool)
import Models (Plant)
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

discoverHandler :: ConnectionPool -> MVar (Set Text) -> Handler [Plant]
discoverHandler db _online = listPlants db

infoHandler :: ConnectionPool -> Text -> Handler (Maybe Plant)
infoHandler = findPlant

labelHandler :: ConnectionPool -> Text -> LabelReq -> Handler ()
labelHandler db plant LabelReq {label} = labelPlant db plant label

server :: FilePath -> ConnectionPool -> Commander -> MVar (Set Text) -> Server AppApi
server static db cmd clients =
  waterHandler cmd
    :<|> addHandler cmd
    :<|> infoHandler db
    :<|> labelHandler db
    :<|> discoverHandler db clients
    :<|> serveDirectoryFileServer static

app :: ConnectionPool -> Topics -> MQTTClient -> Chan LByteString -> MVar (Set Text) -> IO Application
app db topics@Topics {responseTopic} mc msgs clients = do
  -- directory to serve static files from
  static <- getDataDir
  -- init command handler
  let commander = runCommand topics mc msgs
  -- subscribe to command response topic
  print =<< subscribe mc [(topic, options)] []
  -- build app description
  pure $ simpleCors $ serve appApi $ server static db commander clients
  where
    topic = fromString . toString $ responseTopic
    options = subOptions {_subQoS = QoS1}
