module Models where

import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Plant
    name Text
    label Text
    UniquePlant name
    deriving Eq Show Read Generic
|]

instance FromJSON Plant
instance ToJSON Plant
