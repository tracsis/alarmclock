{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.AlarmClock
import Control.Exception
import Data.IORef
import Data.Time
import Text.Printf

printWithTime :: String -> IO ()
printWithTime s = do
  t <- getCurrentTime
  putStrLn $ printf "%-32s: %s" (show t) s

alarmAction :: IORef Bool -> IO (Maybe UTCTime)
alarmAction v = do
  printWithTime "alarm went off"
  readIORef v >>= \case
    False -> do
      printWithTime "not resetting alarm"
      return Nothing
    True -> do
      t <- addUTCTime 5 <$> getCurrentTime
      printWithTime $ printf "alarm reset for %s" $ show t
      return $ Just t

setAlarmLog :: AlarmClock -> UTCTime -> IO ()
setAlarmLog ac t = do
  printWithTime $ printf "alarm set for %s" $ show t
  setAlarm ac t

setAlarmNowLog :: AlarmClock -> IO ()
setAlarmNowLog ac = do
  printWithTime "alarm set for now"
  setAlarmNow ac

main :: IO ()
main = do
  v <- newIORef True
  bracket (newAlarmClock $ alarmAction v) destroyAlarmClock $ \ac -> do
    t <- getCurrentTime
    mask $ \_ -> do
      setAlarmLog ac $ addUTCTime       2  t
      setAlarmLog ac $ addUTCTime (pred 2) t
      setAlarmLog ac $ addUTCTime (succ 2) t
    threadDelay 500000
    setAlarmLog ac $ addUTCTime 4 t
    threadDelay 1900000
    setAlarmNowLog ac
    threadDelay 8000000
    printWithTime "cancelling alarm repeat"
    writeIORef v False
    threadDelay 7000000
    setAlarmLog ac $ addUTCTime 1 t
    threadDelay 500000
    setAlarmLog ac $ addUTCTime 20 t
    threadDelay 4000000
    printWithTime "done"
