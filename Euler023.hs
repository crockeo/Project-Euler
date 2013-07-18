-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 23
-- Time started: 7/16/2013 - 11:40 PM
-- Time ended: 7/17/2013 - 3:55 PM
-------------------------------

-- Change the N to the current problem
module Euler23 where

import Data.List

-- The hard limit of numbers that
-- cannot be represented as the sum of
-- two abundant numbers
cap :: Int
cap = 20161

-- Checking if an element is present
-- more quickly than elem, assuming
-- the list is sorted
elemQuick :: (Eq a, Ord a) => a -> [a] -> Bool
elemQuick n [] = False
elemQuick n list
  | n < center  = elemQuick n $ fst $ splitAt centerpos list
  | n > center  = elemQuick n $ snd $ splitAt (centerpos + 1) list
  | n == center = True
  | otherwise   = False
  where centerpos = (length list) `div` 2
        center = list !! centerpos

-- Getting the list of divisors
divisorsRaw :: Int -> Int -> [Int]
divisorsRaw n x
  | (x ^ 2 < n) && (n `mod` x == 0) = x : (n `div` x) : divisorsRaw n (x + 1)
  | (x ^ 2 < n)                     = divisorsRaw n (x + 1)
  | (x ^ 2 == n)                    = [x]
  | otherwise                       = []

divisors :: Int -> [Int]
divisors 1 = []
divisors n = 1 : (divisorsRaw n 2)

-- Checking if a number is abundant
isAbundantNumber :: Int -> Bool
isAbundantNumber n = (sum $ divisors n) > n

-- List of abundant numbers
abundantNumbers :: [Int]
abundantNumbers = [n | n <- [1 .. cap], isAbundantNumber n]

-- Checking if a number is the sum of two abundant numbers
startX :: Int -> Int
startX n = (head [x | x <- [0 .. (length abundantNumbers) - 1], ((abundantNumbers !! x) * 2) >= n])

isAbundantSumRaw :: Int -> Int -> Bool
isAbundantSumRaw n x
  | x >= length abundantNumbers = False
  | a >= n = False
  | elemQuick (n - a) $ take (x + 1) abundantNumbers = True
  | otherwise = isAbundantSumRaw n (x + 1)
  where a = abundantNumbers !! x

isAbundantSum :: Int -> Bool
isAbundantSum n
  | n > 48 && n `mod` 2 == 0 = True
  | otherwise                = isAbundantSumRaw n $ startX n

-- List of abundant sums
notAbundantSums :: [Int]
notAbundantSums = [n | n <- [1 .. cap], not $ isAbundantSum n]

----
-- NOTE: The algorithm takes a bit of
--       time, if you (whoever is view this)
--       have any ideas on how to improve performance
--       then please message me at oprowarrior@gmail.com


-- Call the main function to get the result
main = sum notAbundantSums