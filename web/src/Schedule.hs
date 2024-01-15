module Schedule where

import Commands (Command (Drive), Commander)
import Control.Concurrent (ThreadId, killThread)
import Control.Lens ((^.))
import Data.Map.Strict qualified as Map
import Models (Plant, name, waterCron, waterVolume)
import System.Cron (MonadSchedule (addJob), execSchedule)

type Schedules = MVar (Map Text (Maybe ThreadId))

schedulePlant :: Commander -> Plant -> IO (Maybe (Text, Maybe ThreadId))
schedulePlant _cmd plant | isNothing $ plant ^. waterCron = pure Nothing
schedulePlant cmds plant = do
  -- build watering command
  let waterCmd = cmds (plant ^. name) (Drive $ plant ^. waterVolume) >> pass -- TODO: signal error
  -- possibly fails to schedule (parse cron)
  mTid <- viaNonEmpty head <$> execSchedule (forM_ (plant ^. waterCron) (addJob waterCmd))
  -- we return the thread id and name pair
  pure $ Just (plant ^. name, mTid)

schedulePlants :: Commander -> [Plant] -> IO Schedules
schedulePlants cmd plants = (newMVar . fromList) =<< mapMaybeM (schedulePlant cmd) plants

reschedulePlant :: Commander -> Schedules -> Plant -> IO (Maybe ())
reschedulePlant cmds scheds plant = do
  -- take out current state
  schedules <- readMVar scheds
  -- possibly kill existing thread
  forM_ (join . Map.lookup (plant ^. name) $ schedules) killThread
  -- reschedule it
  schedRes <- join <$> (snd <<$>> schedulePlant cmds plant)
  -- update the schedules state
  putMVar scheds $ Map.insert (plant ^. name) schedRes schedules
  -- report success
  pure $ void schedRes
