-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 32
-- Time started: 7/22/2013 - 3:23 PM
-- Time ended: 7/24/2013 - 3:46 PM
-------------------------------

-- Change the N to the current problem
module Euler32 where

import Data.List
import Utils

cap = 10000

pairs :: (Integral a) => a -> [(a, a)]
pairs n =
  [(divs !! n, divs !! (((length divs) - 1) - n)) | n <- [0 .. (length divs) `div` 2]]
  where divs = divisors n

isTriplet :: (Integral a) => a -> a -> Bool
isTriplet a b =
  (numLength joined == 9) && (isPandigital joined)
  where joined = joinBigNums [a, b, a * b] 

triplets :: (Integral a) => a -> [(a, a, a)]
triplets n = [(a, b, n) | (a, b) <- pairs n, isTriplet a b]

isNum :: (Integral a) => a -> Bool
isNum n = triplets n /= []

-- Call the main function to get the result
main = sum $ [n | n <- [1 .. cap], isNum n]