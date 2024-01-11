module Api (
  AppApi,
  appApi,
  Command (..),
  Client (..),
  Request (..),
  Response (..),
  AddReq (..),
  LabelReq (..),
  OnlinePlant (..),
) where

import Data.Aeson (FromJSON, Options (..), SumEncoding (..), ToJSON (..), defaultOptions, genericToJSON)
import Data.Aeson.Casing (snakeCase)
import Models qualified
import Servant
import Web.FormUrlEncoded (FromForm)

type Plant = Capture "plant" Text

type WaterAPI = "water" :> QueryParam "t" Word32 :> Post '[JSON] Text

type AddAPI = "add" :> ReqBody '[FormUrlEncoded, JSON] AddReq :> Post '[JSON] Text

type InfoAPI = "info" :> Get '[JSON] (Maybe Models.Plant)

type LabelAPI = "label" :> ReqBody '[FormUrlEncoded, JSON] LabelReq :> Post '[JSON] ()

type DiscoverAPI = "discover" :> Get '[JSON] [OnlinePlant]

type AppApi =
  -- TODO: scope these under 'plant'?
  Plant :> WaterAPI
    :<|> Plant :> AddAPI
    :<|> Plant :> InfoAPI
    :<|> Plant :> LabelAPI
    :<|> DiscoverAPI
    :<|> Raw

appApi :: Proxy AppApi
appApi = Proxy

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

newtype LabelReq = LabelReq
  { label :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromForm LabelReq
instance FromJSON LabelReq

data OnlinePlant = OnlinePlant
  { plant :: Models.Plant
  , online :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OnlinePlant

snakeCaseJson :: Options
snakeCaseJson =
  defaultOptions
    { sumEncoding = ObjectWithSingleField
    , constructorTagModifier = snakeCase
    }