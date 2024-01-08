module Main where

import ApiType (app)
import Control.Concurrent (newChan, writeChan)
import Data.Maybe (fromJust)
import Network.MQTT.Client
import Network.URI (parseURI)
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  let uri = fromJust $ parseURI "mqtt://10.10.0.184"
  putTextLn "Running server on port 8081..."
  msgs <- newChan
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback $ msgReceived msgs} uri
  api <- app mc msgs
  run 8081 api
  where
    msgReceived chan _ _ m _ = writeChan chan m
