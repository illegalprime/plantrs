module Config where

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Network.MQTT.Client (Topic)

data Configuration = Configuration
  { _port :: Int
  , _mqtt :: Text
  , _db :: Database
  , _topics :: Topics
  }
  deriving stock (Show, Eq, Generic)
instance FromJSON Configuration
instance ToJSON Configuration

instance Default Configuration where
  def =
    Configuration
      { _port = 8080
      , _mqtt = "mqtt://127.0.0.1"
      , _db = Sqlite "db.sqlite"
      , _topics = def
      }

newtype Database = Sqlite Text
  deriving stock (Show, Eq, Generic)
instance FromJSON Database
instance ToJSON Database

data Topics = Topics
  { _hello :: Text
  , _goodbye :: Text
  , _discover :: Text
  , _response :: Text
  , _request :: Text
  }
  deriving stock (Show, Eq, Generic)
instance FromJSON Topics
instance ToJSON Topics

instance Default Topics where
  def =
    Topics
      { _hello = "plantrs/hello"
      , _goodbye = "plantrs/goodbye"
      , _discover = "plantrs/discover"
      , _response = "plantrs/responses"
      , _request = "plantrs/request"
      }

makeFieldsNoPrefix ''Configuration
makeFieldsNoPrefix ''Topics

toTopic :: Text -> Topic
toTopic = fromString . toString

defaultConfig :: Configuration
defaultConfig = def
