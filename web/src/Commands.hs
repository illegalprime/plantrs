module Commands where

import Config (HasRequest (request), HasResponse (response), Topics, toTopic)
import Control.Concurrent (readChan, writeChan)
import Control.Lens (makeFieldsNoPrefix, (^.))
import Control.Monad.Loops (untilJust)
import Data.Aeson (Options (constructorTagModifier, fieldLabelModifier, sumEncoding), SumEncoding (ObjectWithSingleField), decode, defaultOptions, encode)
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH (deriveJSON)
import Data.Text qualified as Text
import Discovery (Comms)
import System.Random (randomIO)

data Command
  = Add Word32 Word32
  | Drive Word32
  deriving stock (Eq, Show, Generic)

deriveJSON defaultOptions {sumEncoding = ObjectWithSingleField, constructorTagModifier = snakeCase} ''Command

data Request = Request
  { _command :: Command
  , _response_topic :: Text
  , _correlate :: Word64
  }
  deriving stock (Eq, Show, Generic)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Request

data Response = Response
  { _body :: Text
  , _correlate :: Word64
  }
  deriving stock (Eq, Show, Generic)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Response

makeFieldsNoPrefix ''Request
makeFieldsNoPrefix ''Response

type Commander = Text -> Command -> IO Text

runCommand :: Topics -> Comms -> Commander
runCommand topics (rx, tx) plant cmd = do
  -- prepare request
  nonce <- randomIO -- TODO: best way?
  let req = Request cmd (topics ^. response) nonce
  let topic = Text.intercalate "/" [topics ^. request, plant]
  let encoded = encode req
  putLBSLn $ "sending request: " <> encoded
  -- send it
  writeChan tx (toTopic topic, encoded)
  -- TODO add commands timeout
  -- wait for a response
  untilJust $ do
    (_t, msg) <- readChan rx -- TODO: semantics of multiple consumers?
    putLBSLn $ "received message: " <> msg
    case decode msg :: Maybe Response of
      Nothing -> Nothing <$ putLBSLn ("could not parse: " <> msg)
      Just r | r ^. correlate == nonce -> pure $ Just $ r ^. body
      _ -> pure Nothing
