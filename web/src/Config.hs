module Config (
  Configuration (..),
  Topics (..),
  toTopic,
  defaultConfig,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Network.MQTT.Client (Topic)

data Configuration = Configuration
  { port :: Int
  , mqtt :: Text
  , topics :: Topics
  }
  deriving stock (Show, Eq, Generic)
instance FromJSON Configuration
instance ToJSON Configuration

instance Default Configuration where
  def =
    Configuration
      { port = 8080
      , mqtt = "mqtt://127.0.0.1"
      , topics = def
      }

data Topics = Topics
  { helloTopic :: Text
  , goodbyeTopic :: Text
  , discoverTopic :: Text
  , responseTopic :: Text
  , requestTopic :: Text
  }
  deriving stock (Show, Eq, Generic)
instance FromJSON Topics
instance ToJSON Topics

instance Default Topics where
  def =
    Topics
      { helloTopic = "plantrs/hello"
      , goodbyeTopic = "plantrs/goodbye"
      , discoverTopic = "plantrs/discover"
      , responseTopic = "plantrs/responses"
      , requestTopic = "plantrs/request"
      }

toTopic :: Text -> Topic
toTopic = fromString . toString

defaultConfig :: Configuration
defaultConfig = def
