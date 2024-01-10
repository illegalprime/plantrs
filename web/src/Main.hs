module Main where

import ApiType (app)
import Control.Concurrent (modifyMVar_, newChan, writeChan)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Network.MQTT.Client
import Network.MQTT.Topic (toFilter)
import Network.URI (parseURI)
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  -- TODO: configure MQTT url
  let uri = fromJust $ parseURI "mqtt://10.10.0.184"
  -- TODO: configure server port
  putTextLn "Running server on port 8081..."
  -- TODO: save offline status
  clients <- newMVar Set.empty
  msgs <- newChan
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
  api <- app mc msgs clients
  run 8081 api
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
