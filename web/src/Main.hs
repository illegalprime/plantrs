module Main where

import ApiType (app)
import Control.Concurrent (modifyMVar_, newChan, writeChan)
import Data.Aeson (FromJSON)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Network.MQTT.Client
import Network.MQTT.Topic (toFilter)
import Network.URI (parseURI)
import Network.Wai.Handler.Warp
import Text.Printf (printf)

data Configuration = Configuration
  { port :: Int
  , mqtt :: Text
  }
  deriving stock (Show, Eq, Generic)
instance FromJSON Configuration

main :: IO ()
main = do
  -- load configuration file
  Configuration {port, mqtt} <- loadYamlSettingsArgs [] useEnv
  -- TODO: save offline status
  -- list of online clients
  clients <- newMVar Set.empty
  -- messages from the broker
  msgs <- newChan
  -- connect to MQTT broker
  let uri = fromJust $ parseURI $ toString mqtt
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback $ msgReceived msgs clients} uri
  -- subscribe to discovery topic
  print
    =<< subscribe
      mc
      [ (toFilter helloTopic, subOptions {_subQoS = QoS1})
      , (toFilter goodbyeTopic, subOptions {_subQoS = QoS1})
      ]
      []
  -- send initial request for clients
  publish mc discoverTopic "ping" False
  -- build web app
  api <- app mc msgs clients
  -- spawn webserver on port
  putStrLn $ printf "Running server on port %d..." port
  run port api
  where
    -- new client is connected
    msgReceived _ clients _ t m _
      | t == helloTopic =
          modifyMVar_ clients (pure . Set.insert (decodeUtf8 m))
    -- client is disconnected
    msgReceived _ clients _ t m _
      | t == goodbyeTopic =
          modifyMVar_ clients (pure . Set.delete (decodeUtf8 m))
    -- other requests dealt with by http handlers
    msgReceived chan _ _ _ m _ = writeChan chan m

-- TODO: configure topics
helloTopic :: Topic
helloTopic = "plantrs/hello"

goodbyeTopic :: Topic
goodbyeTopic = "plantrs/goodbye"

discoverTopic :: Topic
discoverTopic = "plantrs/discover"
