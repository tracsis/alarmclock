{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

{-| Device for running an action at (i.e. shortly after) a certain time, which
can be used to implement things like time-based cache expiry.

This implementation avoids the use of polling and leans on Haskell's scheduler
to achieve low-latency without lots of computational overhead.

The alarm can be set multiple times, and in this case the alarm will go off at
the earliest requested time. If the alarm is set in the past, the action will
run immediately. When the action runs, it clears all future alarms; the action
can itself set the next alarm time.

To perform time-based cache expiry, create an 'AlarmClock' whose action flushes
any stale entries from the cache and then calls `setAlarm` for the next time
that an entry will expire (if there are any). When expiring entries are added
to the cache, call 'setAlarm' to ensure that they will expire in a timely
fashion.

-}

module Control.Concurrent.AlarmClock
  ( AlarmClock()
  , newAlarmClock
  , newAlarmClock'
  , destroyAlarmClock
  , withAlarmClock
  , setAlarm
  , setAlarmSTM
  , setAlarmNow
  , isAlarmSet
  , isAlarmSetSTM
  , TimeScale
  , MonotonicTime(..)
  ) where

import           Control.Concurrent.Async   (async, wait)
import           Control.Concurrent.STM     (STM, TVar, atomically, modifyTVar',
                                             newTVarIO, readTVar, retry,
                                             writeTVar)
import           Control.Concurrent.Timeout (timeout)
import           Control.Exception          (bracket)
import           Control.Monad.Fix          (mfix)
import           Data.Time                  (UTCTime, diffUTCTime,
                                             getCurrentTime)
import           GHC.Conc                   (labelThread, myThreadId)
import           System.Clock               (Clock (Monotonic), TimeSpec,
                                             diffTimeSpec, getTime,
                                             toNanoSecs)

{-| Abstraction that allows for a choice between the UTC timescale and a
monotonic timescale, which differ in their handling of irregularities such as
clock adjustments and leap seconds.

Alarms set using the 'UTCTime' timescale wait for the system clock to pass the
given time before going off, and account for the clock being adjusted
backwards and for (positive) leap seconds while waiting. If the clock is set
forwards, or a negative leap second occurs, then the alarm may go off later
than expected by an amount that is roughly equal to the adjustment. It is
possible to correct for this by setting the alarm again after the adjustment
has occurred.

The 'Monotonic' timescale cannot be so adjusted, which may be more suitable for
some applications.

Note that the timeliness of the alarm going off is very much on a "best effort"
basis, and there are many environmental factors that could cause the alarm to
go off later than expected.

-}

class TimeScale t where
  getAbsoluteTime   :: IO t
  microsecondsDiff  :: t -> t -> Integer
  earlierOf         :: t -> t -> t

instance TimeScale UTCTime where
  getAbsoluteTime        = getCurrentTime
  earlierOf              = min
  microsecondsDiff t1 t2 = ceiling $ (1000000 *) $ diffUTCTime t1 t2

{-| Representation of system monotonic clock. -}
newtype MonotonicTime = MonotonicTime TimeSpec deriving (Show, Eq, Ord)

instance TimeScale MonotonicTime where
  getAbsoluteTime = MonotonicTime <$> getTime Monotonic
  earlierOf       = min
  microsecondsDiff (MonotonicTime t1) (MonotonicTime t2)
                  = (`div` 1000) $ toNanoSecs $ diffTimeSpec t1 t2

{-| An 'AlarmClock' is a device for running an action at (or shortly after) a certain time. -}
data AlarmClock t = AlarmClock
  { acWaitForExit :: IO ()
  , acNewSetting  :: TVar (AlarmSetting t)
  , acIsSet       :: TVar Bool
  }

{-| Create a new 'AlarmClock' that runs the given action. Initially, there is
no wakeup time set: you must call 'setAlarm' for anything else to happen. -}
newAlarmClock
  :: TimeScale t
  => (AlarmClock t -> IO ())
    -- ^ Action to run when the alarm goes off. The action is provided the alarm clock
    -- so it can set a new alarm if desired. Note that `setAlarm` must be called once
    -- the alarm has gone off to cause it to go off again.
  -> IO (AlarmClock t)
newAlarmClock onWakeUp = newAlarmClock' $ const . onWakeUp

{-| Create a new 'AlarmClock' that runs the given action. Initially, there is
no wakeup time set: you must call 'setAlarm' for anything else to happen. -}
newAlarmClock'
  :: TimeScale t
  => (AlarmClock t -> t -> IO ())
    -- ^ Action to run when the alarm goes off. The action is provided the alarm clock
    -- so it can set a new alarm if desired, and the current time.
    -- Note that `setAlarm` must be called once the alarm has gone off to cause
    -- it to go off again.
  -> IO (AlarmClock t)
newAlarmClock' onWakeUp = mfix $ \ac -> do
  acAsync <- async $ runAlarmClock ac (onWakeUp ac)
  AlarmClock (wait acAsync) <$> newTVarIO AlarmNotSet <*> newTVarIO False

{-| Destroy the 'AlarmClock' so no further alarms will occur. If the alarm is currently going off
then this will block until the action is finished. -}
destroyAlarmClock :: AlarmClock t -> IO ()
destroyAlarmClock AlarmClock{..} = atomically (writeTVar acNewSetting AlarmDestroyed) >> acWaitForExit

{-| The action @withAlarmClock onWakeUp inner@ runs @inner@ with a new 'AlarmClock' which
is destroyed when @inner@ exits. -}
withAlarmClock :: TimeScale t
               => (AlarmClock t -> t -> IO ())
               -> (AlarmClock t -> IO a) -> IO a
withAlarmClock onWakeUp inner = bracket (newAlarmClock' onWakeUp) destroyAlarmClock inner

{-| Make the 'AlarmClock' go off at (or shortly after) the given time.  This
can be called more than once; in which case, the alarm will go off at the
earliest given time. -}
setAlarm :: TimeScale t => AlarmClock t -> t -> IO ()
setAlarm ac t = atomically $ setAlarmSTM ac t

{-| Make the 'AlarmClock' go off at (or shortly after) the given time.  This
can be called more than once; in which case, the alarm will go off at the
earliest given time. -}
setAlarmSTM :: TimeScale t => AlarmClock t -> t -> STM ()
setAlarmSTM AlarmClock{..} t = modifyTVar' acNewSetting $ \case
  AlarmDestroyed -> AlarmDestroyed
  AlarmNotSet    -> AlarmSet t
  AlarmSet t'    -> AlarmSet $! earlierOf t t'

{-| Make the 'AlarmClock' go off right now. -}
setAlarmNow :: TimeScale t => AlarmClock t -> IO ()
setAlarmNow alarm = getAbsoluteTime >>= setAlarm alarm

{-| Is the alarm set - i.e. will it go off at some point in the future even if `setAlarm` is not called? -}
isAlarmSet :: AlarmClock t -> IO Bool
isAlarmSet = atomically . isAlarmSetSTM

{-| Is the alarm set - i.e. will it go off at some point in the future even if `setAlarm` is not called? -}
isAlarmSetSTM :: AlarmClock t -> STM Bool
isAlarmSetSTM AlarmClock{..} = readTVar acNewSetting
  >>= \case { AlarmNotSet -> readTVar acIsSet; _ -> return True }

data AlarmSetting t = AlarmNotSet | AlarmSet t | AlarmDestroyed

labelMyThread :: String -> IO ()
labelMyThread threadLabel = myThreadId >>= flip labelThread threadLabel

runAlarmClock :: TimeScale t => AlarmClock t -> (t -> IO ()) -> IO ()
runAlarmClock AlarmClock{..} wakeUpAction = labelMyThread "alarmclock" >> loop
  where
  loop = readNextSetting >>= handleNewSetting

  readNextSetting = atomically $ readTVar acNewSetting >>= \case
    AlarmNotSet    -> retry
    AlarmDestroyed -> return Nothing
    AlarmSet t     -> do
      writeTVar acNewSetting AlarmNotSet
      writeTVar acIsSet True
      return $ Just t

  handleNewSetting Nothing           = return ()
  handleNewSetting (Just wakeUpTime) = wakeShortlyAfter wakeUpTime

  wakeShortlyAfter wakeUpTime = do
    now <- getAbsoluteTime
    let microsecondsTimeout = microsecondsDiff wakeUpTime now
    if 0 < microsecondsTimeout
      then maybe (wakeShortlyAfter wakeUpTime) handleNewSetting
              =<< timeout microsecondsTimeout readNextSetting
      else do
        atomically $ writeTVar acIsSet False
        wakeUpAction now
        loop
