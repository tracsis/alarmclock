{-# LANGUAGE LambdaCase #-}

{-| Device for running an action at (i.e. shortly after) a certain time, which
    can be used to implement things like time-based cache expiry.

    This implementation avoids the use of polling to achieve low-latency without
    lots of computational overhead.

    The alarm can be set multiple times, and in this case the alarm action
    will run on the earliest such time. If the alarm is set in the past,
    the action will run immediately. When the action runs, it clears all
    future alarms; the action can itself return the time at which it should
    run again.

-}

module Control.Concurrency.AlarmClock where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Timeout
import Data.Time

{-| An 'AlarmClock' is a device for running an action at (or shortly after) a certain time. -}
data AlarmClock = AlarmClock (MVar UTCTime) ThreadId

mkAlarmClock :: IO (Maybe UTCTime) -> IO AlarmClock
mkAlarmClock onWakeUp = do
  mv <- newEmptyMVar
  tid <- mask $ \restore -> forkIO $ runAlarmClock mv $ void $ forkIO $
    restore onWakeUp >>= \case Nothing -> return ()
                               Just wakeUpTime -> setAlarmVar mv wakeUpTime 
  return $ AlarmClock mv tid

destroyAlarmClock :: AlarmClock -> IO ()
destroyAlarmClock (AlarmClock _ tid) = killThread tid

setAlarmVar :: MVar UTCTime -> UTCTime -> IO ()
setAlarmVar mv wakeUpTime = tryTakeMVar mv >>= \case
  Nothing          -> putMVar mv      wakeUpTime
  Just wakeUpTime' -> putMVar mv (min wakeUpTime wakeUpTime')

setAlarm :: AlarmClock -> UTCTime -> IO ()
setAlarm (AlarmClock mv _) = setAlarmVar mv

runAlarmClock :: MVar UTCTime -> IO () -> IO ()
runAlarmClock wakeUpTimeVar wakeUpAction = alarmNotSet
  where
  alarmNotSet = takeMVar wakeUpTimeVar >>= alarmSet

  alarmSet wakeUpTime = do
    t <- getCurrentTime
    let dt = diffUTCTime wakeUpTime t
    if dt < 0 then wakeUpAction >> alarmNotSet
              else do
      let dt_usec = ceiling (1000000 * dt) :: Integer
      let dt_usec_int = if dt_usec > fromIntegral (maxBound :: Int) then maxBound else fromIntegral dt_usec
      timeout dt_usec_int (takeMVar wakeUpTimeVar) >>= \case
        Nothing -> do
          t' <- getCurrentTime
          if t' < wakeUpTime then alarmSet wakeUpTime else wakeUpAction >> alarmNotSet
        Just wakeUpTime' -> alarmSet (min wakeUpTime wakeUpTime')

