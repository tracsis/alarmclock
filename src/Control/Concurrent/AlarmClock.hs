{-# LANGUAGE LambdaCase #-}

{-| Device for running an action at (i.e. shortly after) a certain time, which
can be used to implement things like time-based cache expiry.

This implementation avoids the use of polling and leans on Haskell's scheduler
to achieve low-latency without lots of computational overhead.

The alarm can be set multiple times, and in this case the alarm will go off at
the earliest requested time. If the alarm is set in the past, the action will
run immediately. When the action runs, it clears all future alarms; the action
can itself return the time at which it should run again.

To perform time-based cache expiry, create an 'AlarmClock' whose action flushes
any stale entries from the cache and returns the next time that an entry will
expire. If the cache contains no entries that will expire, return 'Nothing'
from the alarm action. When expiring entries are added to the cache, call
'setAlarm' to ensure that they will expire in a timely fashion.

-}

module Control.Concurrent.AlarmClock
  ( AlarmClock()
  , newAlarmClock
  , destroyAlarmClock
  , setAlarm
  , setAlarmNow
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Time
import System.Timeout

{-| An 'AlarmClock' is a device for running an action at (or shortly after) a certain time. -}
data AlarmClock = AlarmClock (IO ()) (TVar AlarmSetting)

{-| Create a new 'AlarmClock' that runs the given action. Initially, there is
no wakeup time set: you must call 'setAlarm' for anything else to happen. -}
newAlarmClock
  :: (AlarmClock -> IO ())
    -- ^ Action to run when the alarm goes off. The action is provided the alarm clock
    -- so it can set a new alarm if desired. Note that `setAlarm` must be called once
    -- the alarm has gone off to cause it to go off again.
  -> IO AlarmClock
newAlarmClock onWakeUp = do
  joinVar <- atomically $ newTVar False
  ac <- atomically $ AlarmClock (waitOn joinVar) <$> newTVar AlarmNotSet
  void $ mask $ \restore -> forkIO $ runAlarmClock ac (restore $ onWakeUp ac) `finally` atomically (writeTVar joinVar True)
  return ac

waitOn :: TVar Bool -> IO ()
waitOn v = atomically $ readTVar v >>= \case True -> return (); False -> retry

{-| Destroy the 'AlarmClock' so no further alarms will occur. If the alarm is currently going off
then this will block until the action is finished. -}
destroyAlarmClock :: AlarmClock -> IO ()
destroyAlarmClock (AlarmClock j q) = atomically (writeTVar q AlarmDestroyed) >> j

{-| Make the 'AlarmClock' go off at (or shortly after) the given time.  This
can be called more than once; in which case, the alarm will go off at the
earliest given time. -}
setAlarm :: AlarmClock -> UTCTime -> IO ()
setAlarm (AlarmClock _ q) t = atomically $ modifyTVar' q $ \case
  AlarmDestroyed -> AlarmDestroyed
  AlarmNotSet -> AlarmSet t
  AlarmSet t' -> AlarmSet $! min t t'

{-| Make the 'AlarmClock' go off right now. -}
setAlarmNow :: AlarmClock -> IO ()
setAlarmNow alarm = getCurrentTime >>= setAlarm alarm

data AlarmSetting = AlarmNotSet | AlarmSet UTCTime | AlarmDestroyed

readNextAlarmSetting :: AlarmClock -> IO (Maybe UTCTime)
readNextAlarmSetting (AlarmClock _ q) = atomically $ readTVar q >>= \case
  AlarmNotSet    -> retry
  AlarmDestroyed -> return Nothing
  AlarmSet t     -> writeTVar q AlarmNotSet >> return (Just t)

runAlarmClock :: AlarmClock -> IO () -> IO ()
runAlarmClock ac wakeUpAction = loop
  where
  loop = readNextAlarmSetting ac >>= go

  go Nothing           = return ()
  go (Just wakeUpTime) = wakeNoLaterThan wakeUpTime

  wakeNoLaterThan wakeUpTime = do
    dt <- diffUTCTime wakeUpTime <$> getCurrentTime
    if dt <= 0
      then actAndContinue
      else timeout (fromIntegral $ min maxDelay $ ceiling $ 1000000 * dt)
                   (readNextAlarmSetting ac)
            >>= \case
              Nothing -> do
                t' <- getCurrentTime
                if t' < wakeUpTime
                  then wakeNoLaterThan wakeUpTime
                  else actAndContinue
              Just newSetting -> go newSetting

  actAndContinue = wakeUpAction >> loop

maxDelay :: Integer
maxDelay = fromIntegral (maxBound :: Int)
