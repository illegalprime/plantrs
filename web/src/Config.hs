module Config where

import Control.Error (throwE)
import Control.Lens (makeFieldsNoPrefix, (^.))
import Control.Monad.Except (Except)
import Data.Aeson (Options (fieldLabelModifier), defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Default (Default (def))
import Network.MQTT.Client (Topic)
import Network.MQTT.Topic (mkTopic)

data Configuration t = Configuration
  { _port :: Int
  , _mqtt :: Text
  , _db :: Database
  , _topics :: t
  }
  deriving stock (Show, Eq, Generic)

instance Default (Configuration UnvalidatedTopics) where
  def =
    Configuration
      { _port = 8080
      , _mqtt = "mqtt://127.0.0.1"
      , _db = Sqlite "db.sqlite"
      , _topics = def
      }

newtype Database = Sqlite Text
  deriving stock (Show, Eq, Generic)

data UnvalidatedTopics = UnvalidatedTopics
  { _hello :: Text
  , _goodbye :: Text
  , _discover :: Text
  , _response :: Text
  , _request :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Default UnvalidatedTopics where
  def =
    UnvalidatedTopics
      { _hello = "plantrs/hello"
      , _goodbye = "plantrs/goodbye"
      , _discover = "plantrs/discover"
      , _response = "plantrs/responses"
      , _request = "plantrs/request"
      }

newtype InvalidTopicException = InvalidTopicException Text
  deriving newtype (Show)
instance Exception InvalidTopicException

data Topics = Topics
  { _hello :: Topic
  , _goodbye :: Topic
  , _discover :: Topic
  , _response :: Topic
  , _request :: Topic
  }
  deriving stock (Show, Eq, Generic)

makeFieldsNoPrefix ''Configuration
makeFieldsNoPrefix ''UnvalidatedTopics
makeFieldsNoPrefix ''Topics

defaultConfig :: Configuration UnvalidatedTopics
defaultConfig = def

validateTopic :: Text -> Except InvalidTopicException Topic
validateTopic t = maybe (throwE $ InvalidTopicException t) pure (mkTopic t)

-- TODO: best way to handle config validation?
validateConfig :: Configuration UnvalidatedTopics -> Except InvalidTopicException (Configuration Topics)
validateConfig (Configuration p m d tops) = do
  helloTop <- validateTopic $ tops ^. hello
  goodbyeTop <- validateTopic $ tops ^. goodbye
  discoverTop <- validateTopic $ tops ^. discover
  responseTop <- validateTopic $ tops ^. response
  requestTop <- validateTopic $ tops ^. request
  let validTops = Topics helloTop goodbyeTop discoverTop responseTop requestTop
  pure $ Configuration p m d validTops

deriveJSON defaultOptions ''Database
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''UnvalidatedTopics
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Configuration
