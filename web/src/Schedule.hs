module Schedule where

import Api (AsScheduleStatus (_Scheduled), ScheduleStatus (NoSchedule, ScheduleError, Scheduled))
import Commands (Command (Drive), Commander)
import Control.Concurrent (killThread)
import Control.Lens ((^.), (^?))
import Data.Map.Strict qualified as Map
import Models (Plant, name, waterCron, waterVolume)
import System.Cron (MonadSchedule (addJob), execSchedule)

type Schedules = MVar (Map Text ScheduleStatus)

schedulePlant :: Commander -> Plant -> IO (Text, ScheduleStatus)
schedulePlant _cmd plant | isNothing $ plant ^. waterCron = pure (plant ^. name, NoSchedule)
schedulePlant cmds plant = do
  -- build watering command
  let waterCmd = cmds (plant ^. name) (Drive $ plant ^. waterVolume) >> pass -- TODO: signal error
  -- possibly fails to schedule (parse cron)
  mTid <- viaNonEmpty head <$> execSchedule (forM_ (plant ^. waterCron) (addJob waterCmd))
  -- we return the thread id and name pair
  pure (plant ^. name, maybe ScheduleError Scheduled mTid)

schedulePlants :: Commander -> [Plant] -> IO Schedules
schedulePlants cmd plants = (newMVar . fromList) =<< mapM (schedulePlant cmd) plants

reschedulePlant :: Commander -> Schedules -> Plant -> IO ScheduleStatus
reschedulePlant cmds scheds plant = do
  -- take out current state
  schedules <- readMVar scheds
  -- possibly kill existing thread
  forM_ ((^? _Scheduled) =<< Map.lookup (plant ^. name) schedules) killThread
  -- reschedule it
  (_, schedRes) <- schedulePlant cmds plant
  -- update the schedules state
  _ <- swapMVar scheds $ Map.insert (plant ^. name) schedRes schedules
  -- report success
  pure schedRes
