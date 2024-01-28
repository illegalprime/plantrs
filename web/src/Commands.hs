module Commands where

import BroadcastChan (BroadcastChan, Out)
import BroadcastChan.Throw (readBChan)
import Config (HasRequest (request), HasResponse (response), Topics, validateTopic)
import Control.Concurrent (Chan, writeChan)
import Control.Exception (throwIO)
import Control.Lens ((^.))
import Control.Monad.Except (runExcept)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import MqttApi (Command, HasBody (body), HasCorrelate (correlate), MqttMsg, Request (Request), Response)
import Network.MQTT.Topic (Topic (unTopic))
import System.Random (randomIO)
import System.Timeout (timeout)

type Commander = Text -> Command -> IO Text
type Comms = (IO (BroadcastChan Out MqttMsg), Chan MqttMsg)

data CommandTimeout = CommandTimeout
  deriving stock (Show)
instance Exception CommandTimeout

runCommand :: Topics -> Comms -> Commander
runCommand topics (getSubChan, tx) plant cmd = do
  -- prepare request
  nonce <- randomIO
  let req = Request cmd (unTopic $ topics ^. response) nonce
  -- there should never be an issue converting a plant name to a topic
  plantTopic <- either throwIO pure $ runExcept $ validateTopic plant
  let topic = (topics ^. request) <> plantTopic
  let encoded = encode req
  putTextLn $ unwords ["[tx;", unTopic topic, "]", decodeUtf8 encoded]
  -- grab a sub channel
  rx <- getSubChan
  -- send it
  writeChan tx (topic, encoded)
  -- wait for a response
  mResponse <- timeout 5000000 $ untilJust $ do
    (_t, msg) <- readBChan rx
    putTextLn (unwords ["[rx;", unTopic $ topics ^. response, "]", decodeUtf8 msg])
    case decode msg :: Maybe Response of
      Nothing -> Nothing <$ putLBSLn ("could not parse: " <> msg)
      Just r | r ^. correlate == nonce -> pure $ Just $ r ^. body
      _ -> pure Nothing
  case mResponse of
    Just t -> pure t
    Nothing -> throwIO CommandTimeout
