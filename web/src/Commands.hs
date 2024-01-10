module Commands (
  Commander,
  runCommand,
) where

import Api (Command, Request (Request), Response (Response, body, correlate))
import Config (Topics (Topics, requestTopic, responseTopic), toTopic)
import Control.Concurrent (Chan, readChan)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import Data.Text qualified as Text
import Network.MQTT.Client (MQTTClient, publish)
import System.Random (randomIO)

type Commander = Text -> Command -> IO Text

runCommand :: Topics -> MQTTClient -> Chan LByteString -> Commander
runCommand Topics {responseTopic, requestTopic} mc msgs plant cmd = do
  -- prepare request
  nonce <- randomIO -- TODO: best way?
  let request = Request cmd responseTopic nonce
  let topic = Text.intercalate "/" [requestTopic, plant]
  let encoded = encode request
  putLBSLn $ "sending request: " <> encoded
  -- send it
  publish mc (toTopic topic) encoded False
  -- wait for a response
  untilJust $ do
    msg <- readChan msgs -- TODO: semantics of multiple consumers?
    putLBSLn $ "received message: " <> msg
    case decode msg of
      Nothing -> Nothing <$ putLBSLn ("could not parse: " <> msg)
      Just Response {correlate = c, body = b} | c == nonce -> pure $ Just b
      _ -> pure Nothing
