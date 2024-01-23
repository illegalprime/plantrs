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

type LabelAPI mime reply = "label" :> ReqBody '[FormUrlEncoded, JSON] LabelReq :> Post '[mime] reply

type ScheduleAPI = "schedule" :> ReqBody '[FormUrlEncoded, JSON] ScheduleReq :> Post '[JSON] ()

type SimpleScheduleAPI mime reply = "simple-schedule" :> ReqBody '[FormUrlEncoded, JSON] SimpleScheduleReq :> Post '[mime] reply

type DiscoverAPI = "discover" :> Get '[JSON] [OnlinePlant]

type WatchdogAPI = "health" :> UVerb 'GET '[JSON] HealthResponse

type PlantCardsAPI = "plant-cards" :> Get '[HTML] Html

type DetailPlantAPI = "plant" :> CapturePlant :> Get '[HTML] Html

-- Responses

type HtmxResponse = Headers '[Header "HX-Retarget" Text, Header "HX-Reswap" Text, Header "HX-Refresh" Text] Html

type HealthResponse = '[WithStatus 200 PlantStatuses, WithStatus 500 PlantStatuses]

-- Aggregated API

type AppApi =
  WatchdogAPI
    :<|> CapturePlant :> Htmx WaterAPI
    :<|> CapturePlant :> Json WaterAPI ()
    :<|> CapturePlant :> AddAPI
    :<|> CapturePlant :> InfoAPI
    :<|> CapturePlant :> Htmx LabelAPI
    :<|> CapturePlant :> Json LabelAPI ()
    :<|> CapturePlant :> ScheduleAPI
    :<|> Htmx SimpleScheduleAPI
    :<|> DiscoverAPI
    :<|> PlantCardsAPI
    :<|> Get '[HTML] Html
    :<|> DetailPlantAPI
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

data SimpleScheduleReq = SimpleScheduleReq
  { _volume :: Word32
  , _time :: Text
  , _repeat :: Word32
  , _name :: Text
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
makeFieldsNoPrefix ''SimpleScheduleReq
makeFieldsNoPrefix ''OnlinePlant
makeFieldsNoPrefix ''StatusSummary
makeClassyPrisms ''ScheduleStatus

instance FromForm AddReq
instance FromForm LabelReq
instance FromForm ScheduleReq
instance FromForm SimpleScheduleReq

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''AddReq
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LabelReq
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ScheduleReq
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''SimpleScheduleReq
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''OnlinePlant
deriveToJSON defaultOptions {fieldLabelModifier = drop 1} ''StatusSummary
