module Models where

import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Plant json
    name Text
    label Text
    waterVolume Word32 default=0
    waterCron Text Maybe
    UniquePlant name
    deriving Eq Show Read Generic
|]
