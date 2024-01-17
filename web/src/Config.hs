module Config where

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (Options (fieldLabelModifier), defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Default (Default (def))
import Network.MQTT.Client (Topic)

data Configuration = Configuration
  { _port :: Int
  , _mqtt :: Text
  , _db :: Database
  , _topics :: Topics
  }
  deriving stock (Show, Eq, Generic)

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

data Topics = Topics
  { _hello :: Text
  , _goodbye :: Text
  , _discover :: Text
  , _response :: Text
  , _request :: Text
  }
  deriving stock (Show, Eq, Generic)

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

deriveJSON defaultOptions ''Database
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Topics
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Configuration
