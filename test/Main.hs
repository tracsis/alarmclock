module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.AlarmClock
import Control.Exception
import Data.Time

alarmAction :: IO (Maybe UTCTime)
alarmAction = do
  t <- getCurrentTime
  putStrLn $ "Alarm occurred at " ++ show t
  return $ Just $ addUTCTime 5 t

main :: IO ()
main = bracket (mkAlarmClock alarmAction) destroyAlarmClock $ \ac -> do
  t <- getCurrentTime
  setAlarm ac $ addUTCTime 2 t
  threadDelay 18000000
