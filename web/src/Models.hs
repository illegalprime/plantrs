module Models where

import Data.Time (UTCTime)
import Database.Persist.TH
import MqttApi (Command)

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

ActivityLog json
    time UTCTime
    target Text
    commandName Text
    command Command
    response Text Maybe
    successful Bool
    deriving Eq Show Read Generic
|]
