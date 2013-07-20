-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 26
-- Time started: 7/18/2013 - 2:42 PM
-- Time ended: 7/20/2013 - 12:51 AM
-------------------------------

-- Change the N to the current problem
module Euler26 where

import Data.List

sampleCap :: Int
sampleCap = 1000

getRepeatingPortionRaw :: Int -> Int -> [Int] -> [Int] -> [Int]
getRepeatingPortionRaw a b preva res
  | a == 0       = []
  | elem a preva = dropWhile (\x -> fa `div` b /= x) $ reverse res
  | otherwise    = getRepeatingPortionRaw (fa `mod` b) b (a : preva) ((fa `div` b) : res)
  where fa =
          if a < b
            then a * 10
            else a

getRepeatingPortion :: Int -> Int -> [Int]
getRepeatingPortion a b = getRepeatingPortionRaw a b [] []

repeatingPortions :: [(Int, [Int])]
repeatingPortions = [(b, getRepeatingPortion 1 b) | b <- [1 .. sampleCap - 1]]

-- Call the main function to get the result
main = fst $ maximumBy (\x y -> compare (length $ snd x) (length $ snd y)) repeatingPortions