module Server (
  app,
) where

import Api qualified as A
import Commands (Command (Add, Drive), Commander)
import Control.Lens (Field2 (_2), (^.))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (getCurrentTime)
import Database (findPlant, labelPlant, listPlants, schedulePlant)
import Database.Persist.Sql (ConnectionPool)
import Models qualified as M
import Network.Wai.Middleware.Cors (simpleCors)
import Paths_plantrs (getDataDir)
import Schedule (Schedules, reschedulePlant)
import Servant
import Text.Blaze.Html5 (Html)
import View qualified

waterHandler :: Commander -> Text -> Maybe Word32 -> Handler Text
waterHandler commander plant secs = do
  liftIO $ commander plant $ Drive $ fromMaybe 0 secs

addHandler :: Commander -> Text -> A.AddReq -> Handler Text
addHandler commander plant addReq = do
  liftIO $ commander plant $ Add (addReq ^. A.a) (addReq ^. A.b)

infoHandler :: ConnectionPool -> Text -> Handler (Maybe M.Plant)
infoHandler = findPlant

labelHandler :: ConnectionPool -> Text -> A.LabelReq -> Handler ()
labelHandler db plant req = labelPlant db plant (req ^. A.label)

scheduleHandler :: ConnectionPool -> Commander -> Schedules -> Text -> A.ScheduleReq -> Handler ()
scheduleHandler db cmd scheds name req = do
  schedulePlant db name (req ^. A.cron) (req ^. A.volume)
  mPlant <- findPlant db name
  _status <- liftIO $ forM mPlant $ reschedulePlant cmd scheds
  pass -- TODO: error codes if scheduling was successful (cron validation)

watchdogHandler :: (MonadIO m) => Schedules -> m [A.OnlinePlant] -> m (Union A.HealthResponse)
watchdogHandler schedules readPlants = do
  plants <- readPlants
  scheds <- readMVar schedules
  let toStatus oPlant =
        let pName = oPlant ^. A.plant . M.name
            schedStatus = toSchedStatus $ Map.lookup pName scheds
            schedError = schedStatus == A.ScheduleError
            offlineErr = schedStatus == A.Scheduled && not (oPlant ^. A.online)
         in (pName, A.StatusSummary (oPlant ^. A.online) schedStatus (schedError || offlineErr))
      statuses = map toStatus plants
      anyError = any (^. (_2 . A.error)) statuses
      summary = Map.fromList statuses
  if anyError
    then respond $ WithStatus @500 summary
    else respond $ WithStatus @200 summary
  where
    toSchedStatus :: Maybe (Maybe a) -> A.ScheduleStatus -- TODO: simplify
    toSchedStatus Nothing = A.NoSchedule
    toSchedStatus (Just Nothing) = A.ScheduleError
    toSchedStatus (Just (Just _)) = A.Scheduled

indexHandler :: (MonadIO m) => m [A.OnlinePlant] -> m Html
indexHandler getPlants = do
  time <- liftIO getCurrentTime
  plants <- getPlants
  pure $ View.index plants time

server :: FilePath -> ConnectionPool -> Commander -> Schedules -> MVar (Set Text) -> Server A.AppApi
server static db cmd scheds clients =
  waterHandler cmd
    :<|> addHandler cmd
    :<|> infoHandler db
    :<|> labelHandler db
    :<|> scheduleHandler db cmd scheds
    :<|> readPlants
    :<|> watchdogHandler scheds readPlants
    :<|> indexHandler readPlants
    :<|> serveDirectoryWebApp static
  where
    readPlants = allPlants db clients

app :: ConnectionPool -> Commander -> Schedules -> MVar (Set Text) -> IO Application
app db commander schedules clients = do
  -- directory to serve static files from
  static <- getDataDir
  -- build app description
  pure $ simpleCors $ serve A.appApi $ server static db commander schedules clients

allPlants :: (MonadIO m) => ConnectionPool -> MVar (Set Text) -> m [A.OnlinePlant]
allPlants db onlineState = do
  plants <- listPlants db
  online <- readMVar onlineState
  pure $ map (decorateOnline online) plants
  where
    decorateOnline online plant = A.OnlinePlant plant $ Set.member (plant ^. M.name) online
