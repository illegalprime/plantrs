{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infinitely" #-}
module Discovery where

import BroadcastChan (BroadcastChan, In)
import BroadcastChan.Throw (writeBChan)
import Config (HasDiscover (discover), HasGoodbye (goodbye), HasHello (hello), HasResponse (response), Topics, toTopic)
import Control.Concurrent (Chan, modifyMVar_, readChan, threadDelay, writeChan)
import Control.Exception (Handler (Handler), IOException, catches, throwIO)
import Control.Lens ((^.))
import Data.Set qualified as Set
import Database (newPlant)
import Database.Persist.Sql (ConnectionPool)
import Network.MQTT.Client (MQTTClient, MQTTConfig (_msgCB), MQTTException, MessageCallback (SimpleCallback), QoS (QoS1), SubOptions (_subQoS), Topic, connectURI, mqttConfig, publish, subOptions, subscribe)
import Network.MQTT.Topic (toFilter)
import Network.URI (URI)

type MqttMsg = (Topic, LByteString)
type Comms = (BroadcastChan In MqttMsg, Chan MqttMsg)
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
    catcher e = putStrLn ("ERROR: " <> e) >> threadDelay 1000000
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
  -- subscribe to discovery topic
  print
    =<< subscribe
      mc
      [ (toFilter (toTopic $ topics ^. hello), subOptions {_subQoS = QoS1})
      , (toFilter (toTopic $ topics ^. goodbye), subOptions {_subQoS = QoS1})
      , (toFilter (toTopic $ topics ^. response), subOptions {_subQoS = QoS1})
      ]
      []
  -- send initial request for clients
  publish mc (toTopic $ topics ^. discover) "ping" False

mqttHandler :: Topics -> BroadcastChan In MqttMsg -> NotifyOnline -> MessageCallback
mqttHandler topics tx online =
  SimpleCallback handler
  where
    handler _ topic message _ = case topic of
      t | t == toTopic (topics ^. hello) -> writeChan online (msg, Online)
      t | t == toTopic (topics ^. goodbye) -> writeChan online (msg, Offline)
      t -> writeBChan tx (t, message)
      where
        msg = decodeUtf8 message
