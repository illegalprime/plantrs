module Discovery where

import Config (Topics (Topics, discoverTopic, goodbyeTopic, helloTopic), toTopic)
import Control.Concurrent (Chan, modifyMVar_, writeChan)
import Data.Set qualified as Set
import Database (newPlant)
import Database.Persist.Sql (ConnectionPool)
import Network.MQTT.Client (MQTTClient, MessageCallback (SimpleCallback), QoS (QoS1), SubOptions (_subQoS), publish, subOptions, subscribe)
import Network.MQTT.Topic (toFilter)

initDiscover :: Topics -> MQTTClient -> IO ()
initDiscover topics mc = do
  let Topics {discoverTopic, helloTopic, goodbyeTopic} = topics
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

mqttRouter :: ConnectionPool -> Topics -> Chan LByteString -> MVar (Set Text) -> MessageCallback
mqttRouter db Topics {helloTopic, goodbyeTopic} messages online =
  SimpleCallback router
  where
    -- client is connected
    router _ t msg _
      | t == toTopic helloTopic = do
          let name = decodeUtf8 msg
          -- possibly insert into database
          newPlant db name
          -- add to live clients list
          modifyMVar_ online (pure . Set.insert name)
    -- client is disconnected
    router _ t msg _
      | t == toTopic goodbyeTopic =
          modifyMVar_ online (pure . Set.delete (decodeUtf8 msg))
    -- default case
    router _ _ msg _ = writeChan messages msg
