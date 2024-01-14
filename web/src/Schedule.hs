module Schedule where

import Api (Command (Drive))
import Commands (Commander)
import Control.Concurrent (ThreadId, killThread)
import Data.Map.Strict qualified as Map
import Models (Plant (Plant, plantName, plantWaterCron, plantWaterVolume))
import System.Cron (MonadSchedule (addJob), execSchedule)

type Schedules = MVar (Map Text (Maybe ThreadId))

schedulePlant :: Commander -> Plant -> IO (Maybe (Text, Maybe ThreadId))
schedulePlant _cmd Plant {plantWaterCron = Nothing} = pure Nothing
schedulePlant cmds Plant {plantWaterCron = Just cron, plantName, plantWaterVolume} = do
  -- build watering command
  let waterCmd = cmds plantName (Drive plantWaterVolume) >> pass -- TODO: signal error
  -- possibly fails to schedule (parse cron)
  mTid <- viaNonEmpty head <$> execSchedule (forM_ [cron] (addJob waterCmd))
  -- we return the thread id and name pair
  pure $ Just (plantName, mTid)

schedulePlants :: Commander -> [Plant] -> IO Schedules
schedulePlants cmd plants = (newMVar . fromList) =<< mapMaybeM (schedulePlant cmd) plants

reschedulePlant :: Commander -> Schedules -> Plant -> IO (Maybe ())
reschedulePlant cmds scheds plant@Plant {plantName} = do
  -- take out current state
  schedules <- readMVar scheds
  -- possibly kill existing thread
  forM_ (join . Map.lookup plantName $ schedules) killThread
  -- reschedule it
  schedRes <- join <$> (snd <<$>> schedulePlant cmds plant)
  -- update the schedules state
  putMVar scheds $ Map.insert plantName schedRes schedules
  -- report success
  pure $ void schedRes
