module Server (
  app,
  AppEnv (..),
) where

import Api (ScheduleStatus)
import Api qualified as A
import Commands (Command (Add, Drive), Commander)
import Control.Exception (try)
import Control.Lens (Field2 (_2), makeFieldsNoPrefix, view, (^.))
import Control.Lens.Extras (is)
import Data.Map.Strict qualified as Map
import Data.Time (TimeZone, UTCTime, diffUTCTime, getCurrentTime, getCurrentTimeZone, nominalDiffTimeToSeconds)
import Database (findPlant, labelPlant, schedulePlant)
import Database.Persist.Sql (ConnectionPool)
import Models qualified as M
import Schedule (Schedules, reschedulePlant)
import Servant
import Text.Blaze.Html5 (Html)
import Views.Common qualified as Common
import Views.Detail qualified as Detail
import Views.Overview qualified as Overview

-- App Monad

data AppEnv = AppEnv
  { _database :: ConnectionPool
  , _commander :: Commander
  , _schedules :: Schedules
  , -- TODO: can the bottom two be unified?
    _allPlants :: IO [A.OnlinePlant]
  , _onlinePlant :: Text -> IO (Maybe A.OnlinePlant)
  , _static :: FilePath
  }

makeFieldsNoPrefix ''AppEnv

type AppM = ReaderT AppEnv Handler

-- Handlers

waterHandler :: Text -> Maybe Word32 -> AppM ()
waterHandler plant secs = do
  cmds <- view commander
  liftIO $ void $ cmds plant $ Drive $ fromMaybe 0 secs

htmxWaterHandler :: Text -> Maybe Text -> Maybe Word32 -> AppM A.HtmxResponse
htmxWaterHandler plant _header secs = do
  toastHtmx $ "Success." <$ waterHandler plant secs

addHandler :: Text -> A.AddReq -> AppM Text
addHandler plant addReq = do
  cmds <- view commander
  liftIO $ cmds plant $ Add (addReq ^. A.a) (addReq ^. A.b)

infoHandler :: Text -> AppM (Maybe M.Plant)
infoHandler name = do
  db <- view database
  findPlant db name

htmxLabelHandler :: Text -> Maybe Text -> A.LabelReq -> AppM A.HtmxResponse
htmxLabelHandler plant _h req = do
  toastHtmx $ "Renamed." <$ labelHandler plant req

labelHandler :: Text -> A.LabelReq -> AppM ()
labelHandler plant req = do
  db <- view database
  labelPlant db plant (req ^. A.label)

scheduleHandler :: Text -> A.ScheduleReq -> AppM (Union A.ScheduleResponse)
scheduleHandler name req = do
  db <- view database
  cmd <- view commander
  scheds <- view schedules
  schedulePlant db name (req ^. A.cron) (req ^. A.volume) >>= \case
    Left err -> respond $ WithStatus @400 (toText err) -- parse error
    Right () -> do
      mPlant <- findPlant db name
      status <- liftIO $ forM mPlant $ reschedulePlant db cmd scheds
      case status of
        Nothing -> respond $ WithStatus @404 () -- no plant
        Just (A.Scheduled _) -> respond $ WithStatus @200 () -- success
        Just A.ScheduleError -> respond $ WithStatus @400 ("schedule error" :: Text)
        Just A.NoSchedule -> respond $ WithStatus @400 ("failed to update" :: Text)

watchdogHandler :: AppM (Union A.HealthResponse)
watchdogHandler = do
  plants <- liftIO =<< view allPlants
  scheds <- readMVar =<< view schedules
  now <- liftIO getCurrentTime
  let statuses = map (plantSummary scheds now) plants
      anyError = any (^. (_2 . A.error)) statuses
      summary = Map.fromList statuses
  if anyError
    then respond $ WithStatus @500 summary
    else respond $ WithStatus @200 summary

buildSummary :: ([A.OnlinePlant] -> (UTCTime, TimeZone) -> Html) -> AppM Html
buildSummary f = do
  plants <- liftIO =<< view allPlants
  now <- liftIO timeInfo
  pure $ f plants now

detailHandler :: Text -> AppM Html
detailHandler name = do
  findOnline <- view onlinePlant
  mPlant <- liftIO $ findOnline name
  now <- liftIO timeInfo
  case mPlant of
    Just p -> pure $ Detail.index now p
    Nothing -> throwError err404

timeInfo :: IO (UTCTime, TimeZone)
timeInfo = do
  time <- liftIO getCurrentTime
  zone <- liftIO getCurrentTimeZone
  pure (time, zone)

server :: FilePath -> ServerT A.AppApi AppM
server serveDir = do
  watchdogHandler
    :<|> htmxWaterHandler
    :<|> waterHandler
    :<|> addHandler
    :<|> infoHandler
    :<|> htmxLabelHandler
    :<|> labelHandler
    :<|> scheduleHandler
    :<|> (liftIO =<< view allPlants)
    :<|> buildSummary Overview.plantCards
    :<|> buildSummary Overview.index -- index.html
    :<|> detailHandler
    :<|> serveDirectoryWebApp serveDir

app :: AppEnv -> Application
app env = serve A.appApi $ hoistServer A.appApi (usingReaderT env) $ server (env ^. static)

-- Helpers

plantSummary :: Map Text ScheduleStatus -> UTCTime -> A.OnlinePlant -> (Text, A.StatusSummary)
plantSummary scheds now oPlant =
  (pName, summary)
  where
    tBuf = 30 -- give thirty seconds to execute watering command
    pName = oPlant ^. A.plant . M.name
    lastWatered = oPlant ^. A.plant . M.nextWatering
    waterErr = (> tBuf) . nominalDiffTimeToSeconds . diffUTCTime now <$> lastWatered
    schedStatus = fromMaybe A.ScheduleError $ Map.lookup pName scheds
    schedError = schedStatus == A.ScheduleError
    offlineErr = is A._Scheduled schedStatus && not (oPlant ^. A.online)
    summary =
      A.StatusSummary
        (oPlant ^. A.online)
        (oPlant ^. A.plant . M.waterCron)
        schedStatus
        lastWatered
        (schedError || offlineErr || fromMaybe False waterErr)

toastHtmx :: AppM Text -> AppM A.HtmxResponse
toastHtmx action = do
  ioAction <- runHandler . runReaderT action <$> ask
  ioTry <- liftIO $ try ioAction
  let (isErr, msg) = case ioTry :: Either SomeException (Either ServerError Text) of
        Left err -> (True, show err)
        Right (Left err) -> (True, show err)
        Right (Right rp) -> (False, rp)
  pure $
    addHeader "#toaster" $
      addHeader "beforeend" $
        Common.toast isErr msg
