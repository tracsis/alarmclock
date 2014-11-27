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
import Control.Concurrent.STM.TBMQueue
import Control.Exception
import Control.Monad
import Data.Time
import System.Timeout

{-| An 'AlarmClock' is a device for running an action at (or shortly after) a certain time. -}
newtype AlarmClock = AlarmClock (TBMQueue UTCTime)

{-| Create a new 'AlarmClock' that runs the given action. Initially, there is
no wakeup time set: you must call 'setAlarm' for anything else to happen. -}
newAlarmClock
  :: (AlarmClock -> IO ())
    -- ^ Action to run when the alarm goes off. The action is provided the alarm clock
    -- so it can set a new alarm if desired. Note that `setAlarm` must be called once
    -- the alarm has gone off to cause it to go off again.
  -> IO AlarmClock
newAlarmClock onWakeUp = do
  ac <- atomically $ AlarmClock <$> newTBMQueue 1
  void $ mask $ \restore -> forkIO $ runAlarmClock ac $ restore $ onWakeUp ac
  return ac

{-| Destroy the 'AlarmClock' so no further alarms will occur. If a wakeup is in
progress then it will run to completion. -}
destroyAlarmClock :: AlarmClock -> IO ()
destroyAlarmClock (AlarmClock q) = atomically $ closeTBMQueue q

{-| Make the 'AlarmClock' go off at (or shortly after) the given time.  This
can be called more than once; in which case, the alarm will go off at the
earliest given time. -}
setAlarm :: AlarmClock -> UTCTime -> IO ()
setAlarm (AlarmClock q) = atomically . writeTBMQueue q

{-| Make the 'AlarmClock' go off right now. -}
setAlarmNow :: AlarmClock -> IO ()
setAlarmNow alarm = getCurrentTime >>= setAlarm alarm

data AlarmSetting = AlarmNotSet | AlarmSet UTCTime | AlarmDestroyed

readNextAlarmSetting :: AlarmClock -> IO AlarmSetting
readNextAlarmSetting (AlarmClock q)
  = maybe AlarmDestroyed AlarmSet <$> atomically (readTBMQueue q)

runAlarmClock :: AlarmClock -> IO () -> IO ()
runAlarmClock ac wakeUpAction = go AlarmNotSet
  where
  go AlarmDestroyed = return ()
  go AlarmNotSet    = readNextAlarmSetting ac >>= go
  go (AlarmSet wakeUpTime) = do
    dt <- diffUTCTime wakeUpTime <$> getCurrentTime
    if dt < 0
      then actAndContinue
      else timeout (fromIntegral $ min maxDelay $ ceiling $ 1000000 * dt)
                   (readNextAlarmSetting ac)
            >>= \case
              Nothing -> do
                t' <- getCurrentTime
                if t' < wakeUpTime
                  then go (AlarmSet wakeUpTime)
                  else actAndContinue
              Just newSetting -> go newSetting

  actAndContinue = forkIO wakeUpAction >> go AlarmNotSet

maxDelay :: Integer
maxDelay = fromIntegral (maxBound :: Int)
