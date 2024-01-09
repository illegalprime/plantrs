module ApiType (
  app,
) where

import Control.Concurrent (Chan, readChan)
import Control.Monad.Loops (untilJust)
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Network.MQTT.Client (MQTTClient, QoS (QoS1), SubOptions (_subQoS), publish, subOptions, subscribe)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import System.Random (randomIO)
import Text.Printf (printf)

responseTopic :: Text
responseTopic = "plantrs/responses"

type Plant = Capture "plant" Text

type WaterAPI = "water" :> QueryParam "t" Word32 :> Post '[JSON] Response

type AddAPI = "add" :> Capture "a" Word32 :> Capture "b" Word32 :> Post '[JSON] Response

type DiscoverAPI = "discover" :> Get '[JSON] [Text]

type Api =
  Plant :> WaterAPI
    :<|> Plant :> AddAPI
    :<|> DiscoverAPI

appApi :: Proxy Api
appApi = Proxy

waterHandler :: Commander -> Text -> Maybe Word32 -> Handler Response
waterHandler commander plant secs = do
  liftIO $ commander plant $ Drive $ fromMaybe 0 secs

addHandler :: Commander -> Text -> Word32 -> Word32 -> Handler Response
addHandler commander plant a b = do
  liftIO $ commander plant $ Add a b

discoverHandler :: MVar (Set Text) -> Handler [Text]
discoverHandler = (toList <$>) . readMVar

server :: Commander -> MVar (Set Text) -> Server Api
server cmd clients =
  waterHandler cmd
    :<|> addHandler cmd
    :<|> discoverHandler clients

type Commander = Text -> Command -> IO Response

runCommand :: MQTTClient -> Chan LByteString -> Commander
runCommand mc msgs plant cmd = do
  -- prepare request
  nonce <- randomIO -- TODO: best way?
  let request = Request cmd responseTopic nonce
  let topic = fromString $ printf "plantrs/%s/request" plant
  let encoded = encode request
  putLBSLn $ "sending request: " <> encoded
  -- send it
  publish mc topic encoded False
  -- wait for a response
  untilJust $ do
    msg <- readChan msgs -- TODO: semantics of multiple consumers?
    putLBSLn $ "received message: " <> msg
    case decode msg of
      Nothing -> Nothing <$ putLBSLn ("could not parse: " <> msg)
      Just r@Response {correlate = c} | c == nonce -> pure $ Just r
      _ -> pure Nothing

app :: MQTTClient -> Chan LByteString -> MVar (Set Text) -> IO Application
app mc msgs clients = do
  -- init command handler
  let commander = runCommand mc msgs
  -- subscribe to command response topic
  print =<< subscribe mc [(topic, options)] []
  -- build app description
  pure $ simpleCors $ serve appApi $ server commander clients
  where
    topic = fromString . toString $ responseTopic
    options = subOptions {_subQoS = QoS1}

data Command
  = Add Word32 Word32
  | Drive Word32
  deriving stock (Eq, Show, Generic)

instance ToJSON Command where
  toJSON = genericToJSON snakeCaseJson

data Request = Request
  { command :: Command
  , response_topic :: Text
  , correlate :: Word64
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Request

data Response = Response
  { body :: Text
  , correlate :: Word64
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

snakeCaseJson :: Options
snakeCaseJson =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    , constructorTagModifier = snakeCase
    }
