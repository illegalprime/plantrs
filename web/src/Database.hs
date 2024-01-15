module Database where

import Database.Persist.Sql (ConnectionPool, Entity (entityVal), PersistQueryRead (exists, selectFirst), PersistQueryWrite (updateWhere), PersistStoreWrite (insert_), liftSqlPersistMPool, selectList, (=.), (==.))
import Models (EntityField (Label, Name, WaterCron, WaterVolume), Plant (Plant))

-- Plants
listPlants :: (MonadIO m) => ConnectionPool -> m [Plant]
listPlants db = flip liftSqlPersistMPool db $ do
  entityVal <<$>> selectList [] []

findPlant :: (MonadIO m) => ConnectionPool -> Text -> m (Maybe Plant)
findPlant db name = flip liftSqlPersistMPool db $ do
  entityVal <<$>> selectFirst [Name ==. name] []

labelPlant :: (MonadIO m) => ConnectionPool -> Text -> Text -> m ()
labelPlant db name label = flip liftSqlPersistMPool db $ do
  updateWhere [Name ==. name] [Label =. label]

newPlant :: (MonadIO m) => ConnectionPool -> Text -> m ()
newPlant db name = flip liftSqlPersistMPool db $ do
  unlessM (exists [Name ==. name]) (insert_ $ Plant name name 0 Nothing)

schedulePlant :: (MonadIO m) => ConnectionPool -> Text -> Text -> Word32 -> m ()
schedulePlant db name cron volume = flip liftSqlPersistMPool db $ do
  updateWhere [Name ==. name] [WaterVolume =. volume, WaterCron =. Just cron]
