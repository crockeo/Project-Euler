-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 43
-- Time started: 9/23/2013 - 12:37 PM
-- Time ended: 9/23/2013 - 2:31 PM
-------------------------------

-- Change the N to the current problem
module Euler43 where

import Data.List
import Utils

pandigitalInRange :: [Integer]
pandigitalInRange = map (joinNum) $ permutations [0 .. 9]

hasProperty :: Integer -> Bool
hasProperty n
  | length sn /= 10 = False
  | otherwise = hasPropertyRaw nums primes
    where sn = splitNum n
          nums = map (joinNum) [takeRange sn (n, n + 2) | n <- [1 .. 7]]

          hasPropertyRaw :: [Integer] -> [Integer] -> Bool
          hasPropertyRaw [] l2             = True
          hasPropertyRaw (x1:xs1) (x2:xs2) = (x1 `divisible` x2) && hasPropertyRaw xs1 xs2

-- Call the main function to get the result
main = sum $ filter (hasProperty) pandigitalInRange