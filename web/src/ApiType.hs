module ApiType (
  app,
) where

import Control.Concurrent (Chan, readChan)
import Control.Monad.Loops (untilJust)
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Network.MQTT.Client (MQTTClient, QoS (QoS1), SubOptions (_subQoS), publish, subOptions, subscribe)
import Network.Wai.Middleware.Cors (simpleCors)
import Paths_plantrs (getDataDir)
import Servant
import System.Random (randomIO)
import Text.Printf (printf)
import Web.FormUrlEncoded (FromForm)

responseTopic :: Text
responseTopic = "plantrs/responses"

type Plant = Capture "plant" Text

type WaterAPI = "water" :> QueryParam "t" Word32 :> Post '[JSON] Text

type AddAPI = "add" :> ReqBody '[FormUrlEncoded, JSON] AddReq :> Post '[JSON] Text

type DiscoverAPI = "discover" :> Get '[JSON] [Client]

type Api =
  Plant :> WaterAPI
    :<|> Plant :> AddAPI
    :<|> DiscoverAPI
    :<|> Raw

appApi :: Proxy Api
appApi = Proxy

waterHandler :: Commander -> Text -> Maybe Word32 -> Handler Text
waterHandler commander plant secs = do
  liftIO $ commander plant $ Drive $ fromMaybe 0 secs

addHandler :: Commander -> Text -> AddReq -> Handler Text
addHandler commander plant AddReq {a, b} = do
  liftIO $ commander plant $ Add a b

discoverHandler :: MVar (Set Text) -> Handler [Client]
discoverHandler = (map toClient . toList <$>) . readMVar
  where
    toClient c = Client {id = c, name = getName c}

server :: FilePath -> Commander -> MVar (Set Text) -> Server Api
server static cmd clients =
  waterHandler cmd
    :<|> addHandler cmd
    :<|> discoverHandler clients
    :<|> serveDirectoryFileServer static

type Commander = Text -> Command -> IO Text

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
      Just Response {correlate = c, body = b} | c == nonce -> pure $ Just b
      _ -> pure Nothing

app :: MQTTClient -> Chan LByteString -> MVar (Set Text) -> IO Application
app mc msgs clients = do
  -- directory to serve static files from
  static <- getDataDir
  -- init command handler
  let commander = runCommand mc msgs
  -- subscribe to command response topic
  print =<< subscribe mc [(topic, options)] []
  -- build app description
  pure $ simpleCors $ serve appApi $ server static commander clients
  where
    topic = fromString . toString $ responseTopic
    options = subOptions {_subQoS = QoS1}

data Client = Client
  { id :: Text
  , name :: Text
  }
  deriving stock (Eq, Show, Generic)
instance ToJSON Client

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

data AddReq = AddReq
  { a :: Word32
  , b :: Word32
  }
  deriving stock (Eq, Show, Generic)

instance FromForm AddReq
instance FromJSON AddReq

snakeCaseJson :: Options
snakeCaseJson =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    , constructorTagModifier = snakeCase
    }

-- TODO: add user-specified name
getName :: Text -> Text
getName "lime-tree" = "Lime Tree"
getName _ = "Unknown"
