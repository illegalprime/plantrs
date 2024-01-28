module MqttApi where

import Control.Lens (makeFieldsNoPrefix)
import Data.Aeson (Options (constructorTagModifier, fieldLabelModifier, sumEncoding), SumEncoding (ObjectWithSingleField), defaultOptions)
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH (deriveJSON)
import Network.MQTT.Client (Topic)

data Command
  = Add Word32 Word32
  | Drive Word32
  deriving stock (Eq, Show, Generic)

data Request = Request
  { _command :: Command
  , _response_topic :: Text
  , _correlate :: Word64
  }
  deriving stock (Eq, Show, Generic)

data Response = Response
  { _body :: Text
  , _correlate :: Word64
  }
  deriving stock (Eq, Show, Generic)

deriveJSON defaultOptions {sumEncoding = ObjectWithSingleField, constructorTagModifier = snakeCase} ''Command
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Request
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Response

makeFieldsNoPrefix ''Request
makeFieldsNoPrefix ''Response

type MqttMsg = (Topic, LByteString)
