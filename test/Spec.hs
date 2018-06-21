{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Hspec

import           Control.Concurrent
import           Control.Concurrent.AlarmClock.TimeScale
import           Data.Proxy
import           Data.Time

main :: IO ()
main = hspec $ describe "Control.Concurrent.AlarmClock" $ do

  describe "Timescale" $ do
    let beforeTimescaleTest :: TimeScale t => IO (t,t)
        beforeTimescaleTest = do
          putStrLn "getting first time"
          t0 <- getAbsoluteTime
          threadDelay 500000
          putStrLn "getting second time"
          t1 <- getAbsoluteTime
          putStrLn "got times"
          return (t0, t1)

        timescaleSpec :: (TimeScale t, Eq t, Show t) => Proxy t -> SpecWith (t,t)
        timescaleSpec _ = do
          describe "earlierOf" $ do
            it "picks the first if earlier"  $ \(t0,t1) -> earlierOf t0 t1 `shouldBe` t0
            it "picks the second if earlier" $ \(t0,t1) -> earlierOf t1 t0 `shouldBe` t0

          describe "microsecondsDiff" $ do
            it "finds a difference of at least 500ms"
              $ \(t0,t1) -> microsecondsDiff t1 t0 `shouldSatisfy` (>= 500000)
            it "finds a difference of no more than 600ms"
              $ \(t0,t1) -> microsecondsDiff t1 t0 `shouldSatisfy` (<= 600000)
            it "finds a negative difference if args reversed"
              $ \(t0,t1) -> microsecondsDiff t0 t1 `shouldSatisfy` (<= (-500000))

    describe "UTCTime"   $ beforeAll beforeTimescaleTest $ timescaleSpec (Proxy :: Proxy UTCTime)
    describe "Monotonic" $ beforeAll beforeTimescaleTest $ timescaleSpec (Proxy :: Proxy MonotonicTime)

  return ()
