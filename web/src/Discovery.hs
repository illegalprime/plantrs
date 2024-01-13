{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}
module Discovery where

import Config (Topics (Topics, discoverTopic, goodbyeTopic, helloTopic, responseTopic), toTopic)
import Control.Concurrent (Chan, modifyMVar_, readChan, threadDelay, writeChan)
import Control.Exception (Handler (Handler), IOException, catches, throwIO)
import Data.Set qualified as Set
import Database (newPlant)
import Database.Persist.Sql (ConnectionPool)
import Network.MQTT.Client (MQTTClient, MQTTConfig (_msgCB), MQTTException, MessageCallback (SimpleCallback), QoS (QoS1), SubOptions (_subQoS), Topic, connectURI, mqttConfig, publish, subOptions, subscribe)
import Network.MQTT.Topic (toFilter)
import Network.URI (URI)

type MqttChan = Chan (Topic, LByteString)
type Comms = (MqttChan, MqttChan)
type NotifyOnline = Chan (Text, OnlineStatus)
data OnlineStatus = Online | Offline

runMqtt :: URI -> Topics -> Comms -> NotifyOnline -> IO ()
runMqtt uri topics (tx, rx) online = do
  -- re-run on exceptions
  forever $
    catches
      server
      [ Handler (\(ex :: MQTTException) -> catcher (show ex))
      , Handler (\(ex :: IOException) -> catcher (show ex))
      ]
  where
    catcher e = putStrLn ("ERROR: " <> e) >> threadDelay 1000000 -- TODO: decay timeout
    requeueMsg chan msg (ex :: MQTTException) = do
      writeChan chan msg
      throwIO ex
    server = do
      -- connect to the MQTT broker
      mc <- connectURI mqttConfig {_msgCB = mqttHandler topics tx online} uri
      -- discover online clients
      initDiscover topics mc
      -- send off new messages as requested
      forever $ do
        -- read next message
        (topic, msg) <- readChan rx
        -- send if off, possibly failing
        flip catches [Handler $ requeueMsg rx (topic, msg)] $ do
          publish mc topic msg False

-- collect online status messages into an 'active' list
foldOnline :: ConnectionPool -> MVar (Set Text) -> NotifyOnline -> IO ()
foldOnline db onlineStatus online = forever $ do
  (name, status) <- readChan online
  case status of
    Online -> do
      newPlant db name -- possibly create a new entry for a new client
      modifyMVar_ onlineStatus (pure . Set.insert name)
    Offline ->
      modifyMVar_ onlineStatus (pure . Set.delete name)

initDiscover :: Topics -> MQTTClient -> IO ()
initDiscover topics mc = do
  let Topics {discoverTopic, helloTopic, goodbyeTopic, responseTopic} = topics
  -- subscribe to discovery topic
  print
    =<< subscribe
      mc
      [ (toFilter (toTopic helloTopic), subOptions {_subQoS = QoS1})
      , (toFilter (toTopic goodbyeTopic), subOptions {_subQoS = QoS1})
      , (toFilter (toTopic responseTopic), subOptions {_subQoS = QoS1})
      ]
      []
  -- send initial request for clients
  publish mc (toTopic discoverTopic) "ping" False

mqttHandler :: Topics -> MqttChan -> NotifyOnline -> MessageCallback
mqttHandler Topics {helloTopic, goodbyeTopic} tx online =
  SimpleCallback handler
  where
    handler _ topic message _ = case topic of
      t | t == toTopic helloTopic -> writeChan online (msg, Online)
      t | t == toTopic goodbyeTopic -> writeChan online (msg, Offline)
      t -> writeChan tx (t, message)
      where
        msg = decodeUtf8 message
