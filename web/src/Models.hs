module Models where

import Data.Time (UTCTime)
import Database.Persist.TH

share
  [mkPersist sqlSettings {mpsGenerateLenses = True, mpsPrefixFields = False}, mkMigrate "migrateAll"]
  [persistLowerCase|
Plant json
    name Text
    label Text
    waterVolume Word32 default=0
    waterCron Text Maybe
    nextWatering UTCTime Maybe
    UniquePlant name
    deriving Eq Show Read Generic
|]
