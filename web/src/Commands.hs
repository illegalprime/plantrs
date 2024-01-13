module Commands (
  Commander,
  runCommand,
) where

import Api (Command, Request (Request), Response (Response, body, correlate))
import Config (Topics (Topics, requestTopic, responseTopic), toTopic)
import Control.Concurrent (readChan, writeChan)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import Data.Text qualified as Text
import Discovery (Comms)
import System.Random (randomIO)

type Commander = Text -> Command -> IO Text

runCommand :: Topics -> Comms -> Commander
runCommand Topics {responseTopic, requestTopic} (rx, tx) plant cmd = do
  -- prepare request
  nonce <- randomIO -- TODO: best way?
  let request = Request cmd responseTopic nonce
  let topic = Text.intercalate "/" [requestTopic, plant]
  let encoded = encode request
  putLBSLn $ "sending request: " <> encoded
  -- send it
  writeChan tx (toTopic topic, encoded)
  -- wait for a response
  untilJust $ do
    (_t, msg) <- readChan rx -- TODO: semantics of multiple consumers?
    putLBSLn $ "received message: " <> msg
    case decode msg of
      Nothing -> Nothing <$ putLBSLn ("could not parse: " <> msg)
      Just Response {correlate = c, body = b} | c == nonce -> pure $ Just b
      _ -> pure Nothing
