module Main where

import Data.Time.Clock

-- Getting the current time in Double form
time :: IO Double
time = do
  currTime <- (getCurrentTime >>= (return . utctDayTime))
  return (fromRational $ toRational currTime :: Double)

-- Finding how long it takes to perform a function
clock :: IO () -> IO Double
clock fn = do
  sTime <- time
  fn
  eTime <- time

  return (eTime - sTime)