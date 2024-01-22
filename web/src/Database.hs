module Database where

import Api qualified as A
import Control.Lens ((^.))
import Data.Set qualified as Set
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sql (ConnectionPool, Entity (entityVal), PersistQueryRead (exists, selectFirst), PersistQueryWrite (updateWhere), PersistStoreWrite (insert_), liftSqlPersistMPool, selectList, (=.), (==.))
import Models (EntityField (Label, Name, NextWatering, WaterCron, WaterVolume), Plant (Plant))
import Models qualified as M
import System.Cron (nextMatch, parseCronSchedule)

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
  unlessM (exists [Name ==. name]) (insert_ $ Plant name name 0 Nothing Nothing)

schedulePlant :: (MonadIO m) => ConnectionPool -> Text -> Text -> Word32 -> m (Either String ())
schedulePlant db name cron volume = flip liftSqlPersistMPool db $ nextWater cron $ \t -> do
  updateWhere [Name ==. name] [WaterVolume =. volume, WaterCron =. Just cron, NextWatering =. Just t]

bumpPlant :: (MonadIO m) => ConnectionPool -> Text -> Text -> m (Either String ())
bumpPlant db name cron = flip liftSqlPersistMPool db $ nextWater cron $ \t -> do
  updateWhere [Name ==. name] [NextWatering =. Just t]

nextWater :: (MonadIO m) => Text -> (UTCTime -> m ()) -> m (Either String ())
nextWater cron f = do
  now <- liftIO getCurrentTime
  case flip nextMatch now <$> parseCronSchedule cron of
    Left err -> pure $ Left err
    Right Nothing -> pure $ Left "no next watering"
    Right (Just t) -> Right <$> f t

listOnline :: (MonadIO m) => ConnectionPool -> MVar (Set Text) -> m [A.OnlinePlant]
listOnline db onlineState = do
  decorateOnline onlineState =<< listPlants db

findOnline :: (MonadIO m) => ConnectionPool -> MVar (Set Text) -> Text -> m (Maybe A.OnlinePlant)
findOnline db onlineState name = do
  decorateOnline onlineState =<< findPlant db name

decorateOnline :: (MonadIO m, Functor f) => MVar (Set Text) -> f Plant -> m (f A.OnlinePlant)
decorateOnline onlineState fPlant = do
  online <- readMVar onlineState
  pure $ appendOnline online <$> fPlant
  where
    appendOnline online plant = A.OnlinePlant plant $ Set.member (plant ^. M.name) online
