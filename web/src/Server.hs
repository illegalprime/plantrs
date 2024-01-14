module Server (
  app,
) where

import Api
import Commands (Commander)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Time (getCurrentTime)
import Database (findPlant, labelPlant, listPlants, schedulePlant)
import Database.Persist.Sql (ConnectionPool)
import Models (Plant (Plant, plantName))
import Network.Wai.Middleware.Cors (simpleCors)
import Paths_plantrs (getDataDir)
import Schedule (Schedules, reschedulePlant)
import Servant
import Text.Blaze.Html5 (Html)
import View qualified

waterHandler :: Commander -> Text -> Maybe Word32 -> Handler Text
waterHandler commander plant secs = do
  liftIO $ commander plant $ Drive $ fromMaybe 0 secs

addHandler :: Commander -> Text -> AddReq -> Handler Text
addHandler commander plant AddReq {a, b} = do
  liftIO $ commander plant $ Add a b

infoHandler :: ConnectionPool -> Text -> Handler (Maybe Plant)
infoHandler = findPlant

labelHandler :: ConnectionPool -> Text -> LabelReq -> Handler ()
labelHandler db plant LabelReq {label} = labelPlant db plant label

scheduleHandler :: ConnectionPool -> Commander -> Schedules -> Text -> ScheduleReq -> Handler ()
scheduleHandler db cmd scheds name ScheduleReq {volume, cron} = do
  schedulePlant db name cron volume
  mPlant <- findPlant db name
  _status <- liftIO $ forM mPlant $ reschedulePlant cmd scheds
  pass -- TODO: error codes if scheduling was successful (cron validation)

watchdogHandler :: (MonadIO m) => Schedules -> m [OnlinePlant] -> m (Union HealthResponse)
watchdogHandler schedules readPlants = do
  -- TODO: error codes and check
  -- TODO: lenses
  -- TODO: rename schedule variants
  plants <- readPlants
  scheds <- readMVar schedules
  let getName Plant {plantName} = plantName
  let toStatus OnlinePlant {online, plant} =
        let name = getName plant
            schedStatus = toSchedStatus $ Map.lookup name scheds
            errored = (schedStatus == Errored) || (schedStatus == Scheduled && not online)
         in ( name
            , StatusSummary online schedStatus errored
            )
  let statuses = map toStatus plants
  -- TODO: lens
  let anyError = any (\(_, StatusSummary {error = err}) -> err) statuses
  let summary = Map.fromList statuses
  if anyError
    then respond $ WithStatus @500 summary
    else respond $ WithStatus @200 summary
  where
    toSchedStatus :: Maybe (Maybe a) -> ScheduleStatus
    toSchedStatus Nothing = None
    toSchedStatus (Just Nothing) = Errored
    toSchedStatus (Just (Just _)) = Scheduled

indexHandler :: (MonadIO m) => m [OnlinePlant] -> m Html
indexHandler getPlants = do
  time <- liftIO getCurrentTime
  plants <- getPlants
  pure $ View.index plants time

server :: FilePath -> ConnectionPool -> Commander -> Schedules -> MVar (Set Text) -> Server AppApi
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
  pure $ simpleCors $ serve appApi $ server static db commander schedules clients

allPlants :: (MonadIO m) => ConnectionPool -> MVar (Set Text) -> m [OnlinePlant]
allPlants db onlineState = do
  plants <- listPlants db
  online <- readMVar onlineState
  pure $ map (decorateOnline online) plants
  where
    isOnline online Plant {plantName} = Set.member plantName online
    decorateOnline online plant = OnlinePlant {plant, online = isOnline online plant}
