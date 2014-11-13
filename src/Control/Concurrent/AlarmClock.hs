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

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Timeout
import Data.Time

{-| An 'AlarmClock' is a device for running an action at (or shortly after) a certain time. -}
data AlarmClock = AlarmClock (MVar UTCTime) ThreadId

{-| Create a new 'AlarmClock' that runs the given action. Initially, there is
no wakeup time set: you must call 'setAlarm' for anything else to happen. -}
newAlarmClock
  :: IO (Maybe UTCTime)
    -- ^ Action to run when the alarm goes off. The return value, if 'Just', is
    -- used as the next wakeup time. If 'Nothing', the alarm will not wake up again
    -- until 'setAlarm' or 'setAlarmNow' is called, even if 'setAlarm' has previously
    -- been called with a time that is still in the future.
  -> IO AlarmClock
newAlarmClock onWakeUp = do
  mv <- newEmptyMVar
  tid <- mask $ \restore -> forkIO $ runAlarmClock mv $ void $ forkIO $
    restore onWakeUp >>= \case Nothing -> return ()
                               Just wakeUpTime -> setAlarmVar mv wakeUpTime 
  return $ AlarmClock mv tid

{-| Destroy the 'AlarmClock' so no further alarms will occur. If a wakeup is in
progress then it will run to completion. -}
destroyAlarmClock :: AlarmClock -> IO ()
destroyAlarmClock (AlarmClock _ tid) = killThread tid

setAlarmVar :: MVar UTCTime -> UTCTime -> IO ()
setAlarmVar mv wakeUpTime = tryTakeMVar mv >>= \case
  Nothing          -> putMVar mv      wakeUpTime
  Just wakeUpTime' -> putMVar mv (min wakeUpTime wakeUpTime')

{-| Make the 'AlarmClock' go off at (or shortly after) the given time.  This
can be called more than once; in which case, the alarm will go off at the
earliest given time. -}
setAlarm :: AlarmClock -> UTCTime -> IO ()
setAlarm (AlarmClock mv _) = setAlarmVar mv

{-| Make the 'AlarmClock' go off right now. -}
setAlarmNow :: AlarmClock -> IO ()
setAlarmNow alarm = getCurrentTime >>= setAlarm alarm

runAlarmClock :: MVar UTCTime -> IO () -> IO ()
runAlarmClock wakeUpTimeVar wakeUpAction = alarmNotSet
  where
  alarmNotSet = takeMVar wakeUpTimeVar >>= alarmSet

  alarmSet wakeUpTime = do
    t <- getCurrentTime
    let dt = diffUTCTime wakeUpTime t
    if dt < 0 then wakeUpAction >> alarmNotSet
              else timeout (fromIntegral $ min maxDelay $ ceiling $ 1000000 * dt)
                           (takeMVar wakeUpTimeVar) >>= \case
          Nothing -> do
            t' <- getCurrentTime
            if t' < wakeUpTime then alarmSet wakeUpTime else wakeUpAction >> alarmNotSet

          Just wakeUpTime' -> alarmSet (min wakeUpTime wakeUpTime')

maxDelay :: Integer
maxDelay = fromIntegral (maxBound :: Int)
