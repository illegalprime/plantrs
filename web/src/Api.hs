module Api where

import Control.Concurrent (ThreadId)
import Control.Lens (makeClassyPrisms, makeFieldsNoPrefix)
import Data.Aeson (FromJSON, ToJSON (..))
import Models qualified
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)
import Web.FormUrlEncoded (FromForm)

type CapturePlant = Capture "plant" Text

type WaterAPI = "water" :> QueryParam "t" Word32 :> Post '[JSON] Text

type AddAPI = "add" :> ReqBody '[FormUrlEncoded, JSON] AddReq :> Post '[JSON] Text

type InfoAPI = "info" :> Get '[JSON] (Maybe Models.Plant)

type LabelAPI = "label" :> ReqBody '[FormUrlEncoded, JSON] LabelReq :> Post '[JSON] ()

type ScheduleAPI = "schedule" :> ReqBody '[FormUrlEncoded, JSON] ScheduleReq :> Post '[JSON] ()

type DiscoverAPI = "discover" :> Get '[JSON] [OnlinePlant]

type WatchdogAPI = "health" :> UVerb 'GET '[JSON] HealthResponse

type HealthResponse = '[WithStatus 200 PlantStatuses, WithStatus 500 PlantStatuses]

type AppApi =
  CapturePlant :> WaterAPI
    :<|> CapturePlant :> AddAPI
    :<|> CapturePlant :> InfoAPI
    :<|> CapturePlant :> LabelAPI
    :<|> CapturePlant :> ScheduleAPI
    :<|> DiscoverAPI
    :<|> WatchdogAPI
    :<|> Get '[HTML] Html
    :<|> Raw

appApi :: Proxy AppApi
appApi = Proxy

data AddReq = AddReq
  { _a :: Word32
  , _b :: Word32
  }
  deriving stock (Eq, Show, Generic)

instance FromForm AddReq
instance FromJSON AddReq

newtype LabelReq = LabelReq
  { _label :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromForm LabelReq
instance FromJSON LabelReq

data ScheduleReq = ScheduleReq
  { _volume :: Word32
  , _cron :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromForm ScheduleReq
instance FromJSON ScheduleReq

data OnlinePlant = OnlinePlant
  { _plant :: Models.Plant
  , _online :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OnlinePlant

type PlantStatuses = Map Text StatusSummary

data StatusSummary = StatusSummary
  { _online :: Bool
  , _schedule :: ScheduleStatus
  , _error :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON StatusSummary

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
