module Commands where

import BroadcastChan (BroadcastChan, Out)
import BroadcastChan.Throw (readBChan)
import Config (HasRequest (request), HasResponse (response), Topics, validateTopic)
import Control.Concurrent (Chan, writeChan)
import Control.Exception (throwIO, try)
import Control.Lens ((^.))
import Control.Monad.Except (runExcept)
import Control.Monad.Loops (untilJust)
import Data.Aeson (decode, encode)
import Database (logActivity)
import Database.Persist.Sql (ConnectionPool)
import MqttApi (Command, HasBody (body), HasCorrelate (correlate), MqttMsg, Request (Request), Response)
import Network.MQTT.Topic (Topic (unTopic))
import System.Random (randomIO)
import System.Timeout (timeout)

type Commander = Text -> Command -> IO Text
type Comms = (IO (BroadcastChan Out MqttMsg), Chan MqttMsg)

data CommandTimeout = CommandTimeout
  deriving stock (Show)
instance Exception CommandTimeout

newtype CommandError = CommandError Text
  deriving stock (Show)
instance Exception CommandError

runCommand :: ConnectionPool -> Topics -> Comms -> Commander
runCommand db topics comms plant cmd = do
  -- execute request
  res <- try $ runCommand' topics comms plant cmd :: IO (Either SomeException Text)
  -- log success/failure
  logActivity db plant cmd (rightToMaybe res) (isRight res)
  -- rethrow
  either throwIO pure res

runCommand' :: Topics -> Comms -> Commander
runCommand' topics (getSubChan, tx) plant cmd = do
  -- prepare request
  req <- Request cmd (unTopic $ topics ^. response) <$> randomIO
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
      Just r | (r ^. correlate) /= (req ^. correlate) -> pure Nothing
      Just r ->
        if r ^. body == "ok" -- NOTE: only one success response type so far
          then pure $ Just $ r ^. body
          else throwIO $ CommandError $ r ^. body
  maybe (throwIO CommandTimeout) pure mResponse
