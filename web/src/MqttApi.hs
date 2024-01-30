module MqttApi where

import Control.Lens (makeClassyPrisms, makeFieldsNoPrefix)
import Data.Aeson (Options (constructorTagModifier, fieldLabelModifier, sumEncoding), SumEncoding (ObjectWithSingleField), defaultOptions)
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH (deriveJSON)
import Database.Persist.TH (derivePersistFieldJSON)
import Network.MQTT.Client (Topic)

data Command
  = Add Word32 Word32
  | Drive Word32
  deriving stock (Eq, Show, Read, Generic)

commandName :: Command -> Text
commandName (Add _ _) = "add"
commandName (Drive _) = "drive"

data Request = Request
  { _command :: Command
  , _response_topic :: Text
  , _correlate :: Word64
  }
  deriving stock (Eq, Show, Read, Generic)

data Response = Response
  { _body :: Text
  , _correlate :: Word64
  }
  deriving stock (Eq, Show, Read, Generic)

deriveJSON defaultOptions {sumEncoding = ObjectWithSingleField, constructorTagModifier = snakeCase} ''Command
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Request
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Response

makeClassyPrisms ''Command
makeFieldsNoPrefix ''Request
makeFieldsNoPrefix ''Response

derivePersistFieldJSON "Command"
derivePersistFieldJSON "Request"
derivePersistFieldJSON "Response"

type MqttMsg = (Topic, LByteString)
