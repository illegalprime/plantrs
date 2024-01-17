module Api where

import Control.Concurrent (ThreadId)
import Control.Lens (makeClassyPrisms, makeFieldsNoPrefix)
import Data.Aeson (Options (fieldLabelModifier), ToJSON (..), defaultOptions)
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.Time (UTCTime)
import Models qualified
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)
import Web.FormUrlEncoded (FromForm)

-- Helpers

type Htmx route = Header "HX-Request" Text :> route HTML HtmxResponse

type Json route reply = route JSON reply

type CapturePlant = Capture "plant" Text

-- Routes

type WaterAPI mime reply = "water" :> QueryParam "t" Word32 :> Post '[mime] reply

type AddAPI = "add" :> ReqBody '[FormUrlEncoded, JSON] AddReq :> Post '[JSON] Text

type InfoAPI = "info" :> Get '[JSON] (Maybe Models.Plant)

type LabelAPI = "label" :> ReqBody '[FormUrlEncoded, JSON] LabelReq :> Post '[JSON] ()

type ScheduleAPI = "schedule" :> (ReqBody '[FormUrlEncoded, JSON] ScheduleReq :> UVerb 'POST '[JSON] ScheduleResponse)

type DiscoverAPI = "discover" :> Get '[JSON] [OnlinePlant]

type WatchdogAPI = "health" :> UVerb 'GET '[JSON] HealthResponse

-- Responses

type HtmxResponse = Headers '[Header "HX-Retarget" Text, Header "HX-Reswap" Text] Html

type ScheduleResponse = '[WithStatus 200 (), WithStatus 400 Text, WithStatus 404 ()]

type HealthResponse = '[WithStatus 200 PlantStatuses, WithStatus 500 PlantStatuses]

-- Aggregated API

type AppApi =
  WatchdogAPI
    :<|> CapturePlant :> Htmx WaterAPI
    :<|> CapturePlant :> Json WaterAPI ()
    :<|> CapturePlant :> AddAPI
    :<|> CapturePlant :> InfoAPI
    :<|> CapturePlant :> LabelAPI
    :<|> CapturePlant :> ScheduleAPI
    :<|> DiscoverAPI
    :<|> Get '[HTML] Html
    :<|> Raw

appApi :: Proxy AppApi
appApi = Proxy

data AddReq = AddReq
  { _a :: Word32
  , _b :: Word32
  }
  deriving stock (Eq, Show, Generic)

newtype LabelReq = LabelReq
  { _label :: Text
  }
  deriving stock (Eq, Show, Generic)

data ScheduleReq = ScheduleReq
  { _volume :: Word32
  , _cron :: Text
  }
  deriving stock (Eq, Show, Generic)

data OnlinePlant = OnlinePlant
  { _plant :: Models.Plant
  , _online :: Bool
  }
  deriving stock (Eq, Show, Generic)

type PlantStatuses = Map Text StatusSummary

data StatusSummary = StatusSummary
  { _online :: Bool
  , _schedule :: Maybe Text
  , _scheduleStatus :: ScheduleStatus
  , _nextWatering :: Maybe UTCTime
  , _error :: Bool
  }
  deriving stock (Eq, Show, Generic)

data ScheduleStatus
  = NoSchedule
  | ScheduleError
  | Scheduled ThreadId
  deriving stock (Eq, Show, Generic)

instance ToJSON ScheduleStatus where
  toJSON NoSchedule = "none"
  toJSON ScheduleError = "error"
  toJSON (Scheduled _) = "scheduled"

makeFieldsNoPrefix ''AddReq
makeFieldsNoPrefix ''LabelReq
makeFieldsNoPrefix ''ScheduleReq
makeFieldsNoPrefix ''OnlinePlant
makeFieldsNoPrefix ''StatusSummary
makeClassyPrisms ''ScheduleStatus

instance FromForm AddReq
instance FromForm LabelReq
instance FromForm ScheduleReq

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''AddReq
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LabelReq
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ScheduleReq
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''OnlinePlant
deriveToJSON defaultOptions {fieldLabelModifier = drop 1} ''StatusSummary
