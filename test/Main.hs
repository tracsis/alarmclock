{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.AlarmClock
import Control.Exception
import Data.IORef
import Data.Time

alarmAction :: IORef Bool -> IO (Maybe UTCTime)
alarmAction v = do
  t <- getCurrentTime
  putStrLn $ "Alarm occurred at " ++ show t
  (\case True -> Just $ addUTCTime 5 t; False -> Nothing) <$> readIORef v

main :: IO ()
main = do
  v <- newIORef True
  bracket (newAlarmClock $ alarmAction v) destroyAlarmClock $ \ac -> do
    t <- getCurrentTime
    mask $ \_ -> do
      setAlarm ac $ addUTCTime       2  t
      setAlarm ac $ addUTCTime (pred 2) t
      setAlarm ac $ addUTCTime (succ 2) t
    threadDelay 500000
    setAlarm ac $ addUTCTime 4 t
    threadDelay 1500000
    setAlarmNow ac
    threadDelay 8000000
    writeIORef v False
    threadDelay 7000000
    setAlarm ac $ addUTCTime 1 t
    threadDelay 18000000
