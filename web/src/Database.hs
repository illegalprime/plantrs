module Database where

import Database.Persist.Sql (ConnectionPool, Entity (entityVal), PersistQueryRead (exists, selectFirst), PersistQueryWrite (updateWhere), PersistStoreWrite (insert_), liftSqlPersistMPool, selectList, (=.), (==.))
import Models (EntityField (PlantLabel, PlantName), Plant (Plant))

-- Plants
listPlants :: (MonadIO m) => ConnectionPool -> m [Plant]
listPlants db = flip liftSqlPersistMPool db $ do
  entityVal <<$>> selectList [] []

findPlant :: (MonadIO m) => ConnectionPool -> Text -> m (Maybe Plant)
findPlant db name = flip liftSqlPersistMPool db $ do
  entityVal <<$>> selectFirst [PlantName ==. name] []

labelPlant :: (MonadIO m) => ConnectionPool -> Text -> Text -> m ()
labelPlant db name label = flip liftSqlPersistMPool db $ do
  updateWhere [PlantName ==. name] [PlantLabel =. label]

newPlant :: (MonadIO m) => ConnectionPool -> Text -> m ()
newPlant db name = flip liftSqlPersistMPool db $ do
  unlessM (exists [PlantName ==. name]) (insert_ $ Plant name name)
