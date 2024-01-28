module Server (
  app,
  AppEnv (..),
) where

import Commands (Commander)
import Control.Exception (try)
import Control.Lens (Field2 (_2), makeFieldsNoPrefix, view, (^.))
import Control.Lens.Extras (is)
import Data.Map.Strict qualified as Map
import Data.Text (splitOn)
import Data.Time (TimeOfDay (TimeOfDay), TimeZone, UTCTime, diffUTCTime, getCurrentTime, getCurrentTimeZone, localToUTCTimeOfDay, nominalDiffTimeToSeconds)
import Database (findPlant, labelPlant, schedulePlant)
import Database.Persist.Sql (ConnectionPool)
import HttpApi qualified as A
import Models qualified as M
import MqttApi (Command (Add, Drive))
import Schedule (Schedules, reschedulePlant)
import Servant
import Text.Blaze.Html5 (Html)
import Text.Printf (printf)
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
  toastHtmx "Success." $ waterHandler plant secs

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
  toastHtmx "Renamed." $ labelHandler plant req

labelHandler :: Text -> A.LabelReq -> AppM ()
labelHandler plant req = do
  db <- view database
  labelPlant db plant (req ^. A.label)

doSchedule :: Text -> Word32 -> Text -> AppM ()
doSchedule name vol cron = do
  db <- view database
  cmd <- view commander
  scheds <- view schedules
  () <- schedulePlant db name cron vol >>= either (const $ throwError err400) pure
  p <- findPlant db name >>= maybe (throwError err404) pure
  liftIO (reschedulePlant db cmd scheds p) >>= \case
    (A.Scheduled _) -> pass
    A.ScheduleError -> throwError err400
    A.NoSchedule -> throwError err400

scheduleHandler :: Text -> A.ScheduleReq -> AppM ()
scheduleHandler name req = do
  doSchedule name (req ^. A.volume) (req ^. A.cron)

simpleScheduleHandler :: Maybe Text -> A.SimpleScheduleReq -> AppM A.HtmxResponse
simpleScheduleHandler _h req = do
  toastErrorOr runSchedReq (pure signalRefresh)
  where
    runSchedReq = do
      -- parse time string
      (localH, localM :: Word32) <- case readMaybe . toString <$> splitOn ":" (req ^. A.time) of
        [Just hh, Just mm] | hh < 24 && mm < 60 -> pure (hh, mm)
        _ -> throwError err400
      -- adjust time zone
      let tod = TimeOfDay (fromIntegral localH) (fromIntegral localM) 0
      zone <- liftIO getCurrentTimeZone
      let (_, TimeOfDay hh mm _) = localToUTCTimeOfDay zone tod
      -- build cron tab
      let cron = printf "%d %d */%d * *" mm hh (req ^. A.repeat) :: String
      -- apply the new schedule
      doSchedule (req ^. A.name) (req ^. A.volume) (toText cron)

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
    :<|> simpleScheduleHandler
    :<|> (liftIO =<< view allPlants)
    :<|> buildSummary Overview.plantCards
    :<|> buildSummary Overview.index -- index.html
    :<|> detailHandler
    :<|> serveDirectoryWebApp serveDir

app :: AppEnv -> Application
app env = serve A.appApi $ hoistServer A.appApi (usingReaderT env) $ server (env ^. static)

-- Helpers

plantSummary :: Map Text A.ScheduleStatus -> UTCTime -> A.OnlinePlant -> (Text, A.StatusSummary)
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

makeToast :: Bool -> Text -> A.HtmxResponse
makeToast isErr msg =
  addHeader "#toaster" $
    addHeader "beforeend" $
      noHeader $
        Common.toast isErr msg

signalRefresh :: A.HtmxResponse
signalRefresh = noHeader $ noHeader $ addHeader "true" ""

toastErrorOr :: AppM () -> AppM A.HtmxResponse -> AppM A.HtmxResponse
toastErrorOr action fallback = do
  ioAction <- runHandler . runReaderT action <$> ask
  ioTry <- liftIO $ try ioAction
  case ioTry :: Either SomeException (Either ServerError ()) of
    Left err -> pure $ makeToast True $ show err
    Right (Left err) -> pure $ makeToast True $ show err
    Right (Right ()) -> fallback

toastHtmx :: Text -> AppM () -> AppM A.HtmxResponse
toastHtmx success action = toastErrorOr action $ pure $ makeToast False success
