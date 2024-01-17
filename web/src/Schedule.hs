module Schedule where

import Api (AsScheduleStatus (_Scheduled), ScheduleStatus (NoSchedule, ScheduleError, Scheduled))
import Commands (Command (Drive), Commander)
import Control.Concurrent (killThread)
import Control.Lens ((^.), (^?))
import Data.Map.Strict qualified as Map
import Database (bumpPlant)
import Database.Persist.Sql (ConnectionPool)
import Models (Plant (Plant, _waterCron), name, waterCron, waterVolume)
import System.Cron (MonadSchedule (addJob), execSchedule)

type Schedules = MVar (Map Text ScheduleStatus)

schedulePlant :: ConnectionPool -> Commander -> Plant -> IO (Text, ScheduleStatus)
schedulePlant _ _ plant@Plant {_waterCron = Nothing} = pure (plant ^. name, NoSchedule)
schedulePlant db cmds plant@Plant {_waterCron = Just cron} = do
  -- build watering command, failures don't update the db, which the watchdog will find
  let waterCmd =
        cmds (plant ^. name) (Drive $ plant ^. waterVolume)
          >> bumpPlant db (plant ^. name) cron
          >>= print
  -- possibly fails to schedule (parse cron)
  mTid <- viaNonEmpty head <$> execSchedule (forM_ (plant ^. waterCron) (addJob waterCmd))
  -- we return the thread id and name pair
  pure (plant ^. name, maybe ScheduleError Scheduled mTid)

schedulePlants :: ConnectionPool -> Commander -> [Plant] -> IO Schedules
schedulePlants db cmd plants = (newMVar . fromList) =<< mapM (schedulePlant db cmd) plants

reschedulePlant :: ConnectionPool -> Commander -> Schedules -> Plant -> IO ScheduleStatus
reschedulePlant db cmds scheds plant = do
  -- take out current state
  schedules <- readMVar scheds
  -- possibly kill existing thread
  forM_ ((^? _Scheduled) =<< Map.lookup (plant ^. name) schedules) killThread
  -- reschedule it
  (_, schedRes) <- schedulePlant db cmds plant
  -- update the schedules state
  _ <- swapMVar scheds $ Map.insert (plant ^. name) schedRes schedules
  -- report success
  pure schedRes
