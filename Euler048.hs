-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 48
-- Time started: 4/30/2014 - 12:20 PM
-- Time ended: 4/30/2014 - 12:28 PM
-------------------------------

-- Change the N to the current problem
module Euler48 where

-- Calculating the sums of a self-power
calcN :: Integer -> Integer
calcN n =
  sum [x ^ x | x <- [1 .. n]]

-- Splitting a number int a list (returns the number reversed)
splitList :: Integer -> [Integer]
splitList 0 = []
splitList n =
  (n `mod` 10) : (splitList (n `div` 10))

-- Conjoining a (reversed) list into a number
combineList :: [Integer] -> Integer
combineList l =
  combineListRaw l 1
  where combineListRaw  [] _    = 0
        combineListRaw (x:xs) n =
          (x * n) + (combineListRaw xs (n * 10))
          
-- Call the main function to get the result
main =
  combineList $ take 10 $ splitList $ calcN 1000