module Main where

import Config
import Control.Concurrent (modifyMVar_, newChan, writeChan)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Yaml (ToJSON (toJSON))
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Network.MQTT.Client
import Network.MQTT.Topic (toFilter)
import Network.URI (parseURI)
import Network.Wai.Handler.Warp
import Server (app)
import Text.Printf (printf)

main :: IO ()
main = do
  -- load configuration file
  Configuration {port, mqtt, topics} <- loadYamlSettingsArgs [toJSON defaultConfig] useEnv
  let Topics {helloTopic, goodbyeTopic, discoverTopic} = topics
  -- TODO: save offline status
  -- list of online clients
  clients <- newMVar Set.empty
  -- messages from the broker
  msgs <- newChan
  -- connect to MQTT broker
  let uri = fromJust $ parseURI $ toString mqtt
  let callback = msgReceived helloTopic goodbyeTopic msgs clients
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback callback} uri
  -- subscribe to discovery topic
  print
    =<< subscribe
      mc
      [ (toFilter (toTopic helloTopic), subOptions {_subQoS = QoS1})
      , (toFilter (toTopic goodbyeTopic), subOptions {_subQoS = QoS1})
      ]
      []
  -- send initial request for clients
  publish mc (toTopic discoverTopic) "ping" False
  -- build web app
  api <- app topics mc msgs clients
  -- spawn webserver on port
  putStrLn $ printf "Running server on port %d..." port
  run port api
  where
    -- new client is connected
    msgReceived hello _ _ clients _ t m _
      | t == toTopic hello =
          modifyMVar_ clients (pure . Set.insert (decodeUtf8 m))
    -- client is disconnected
    msgReceived _ goodbye _ clients _ t m _
      | t == toTopic goodbye =
          modifyMVar_ clients (pure . Set.delete (decodeUtf8 m))
    -- other requests dealt with by http handlers
    msgReceived _ _ chan _ _ _ m _ = writeChan chan m
