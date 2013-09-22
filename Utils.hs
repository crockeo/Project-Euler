-----------------------------------
-- A file containing a number of --
-- functions I've found helpful  --
-- and reusable in Project Euler --
-----------------------------------
-- Author: Cerek Hillen --
--------------------------

module Utils where

import Data.List

-- Checking if a number is prime
isPrime :: (Integral a) => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = isPrimeRaw n 2
  where isPrimeRaw :: (Integral a) => a -> a -> Bool
        isPrimeRaw n x
          | x * x > n      = True
          | n `mod` x == 0 = False
          | otherwise      = isPrimeRaw n (x + 1)

-- Splitting a number into an array if its digits
splitNum :: (Integral a) => a -> [a]
splitNum n
  | n < 0     = []
  | otherwise = reverse $ splitNumRaw n
  where splitNumRaw :: (Integral a) => a -> [a]
        splitNumRaw 0 = []
        splitNumRaw n = (n `mod` 10) : splitNumRaw (n `div` 10)

-- Joining an array of digits into a number
joinNum :: (Integral a) => [a] -> a
joinNum [] = 0
joinNum (x:xs) = (10 ^ (length xs)) * x + joinNum xs

-- Joining an array of numbers into a single number
joinBigNums :: (Integral a) => [a] -> a
joinBigNums list =
  joinNum $ concat slist
  where slist = map (splitNum) list

-- Getting the length of a number
numLength :: (Integral a) => a -> Int
numLength = length . splitNum

-- A list of factorials
factorials :: (Integral a) => [a]
factorials = scanl (*) 1 [1 ..]

-- A list of fibonacci terms
fibonaccis :: (Integral a) => [a]
fibonaccis = scanl (+) 0 (1 : fibonaccis)

-- A list of primes
primes :: (Integral a) => [a]
primes = [n | n <- [2 ..], isPrime n]

-- Checking if an element is present in a sorted list
quickElem :: (Ord a, Eq a) => a -> [a] -> Bool
quickElem n [] = False
quickElem n (x:xs)
  | n <  x    = False
  | n == x    = True
  | otherwise = quickElem n xs

-- Checking if all elements are present in another list
allElem :: (Eq a) => [a] -> [a] -> Bool
allElem check to = and $ map (\x -> elem x to) check

-- Checking if a number is pandigital
isPandigital :: (Integral a) => a -> Bool
isPandigital n =
  allElem [1 .. fromIntegral $ length sn] sn
  where sn = splitNum n

-- Getting the approximate sqrt of a number floor(sqrt(n))
cleanSqrt :: (Integral a) => a -> a
cleanSqrt n = truncate $ (sqrt . fromIntegral) n

-- Getting the divisors of a number
divisors :: (Integral a) => a -> [a]
divisors n =
  sort $ divisorsRaw n 1
  where divisorsRaw :: (Integral a) => a -> a -> [a]
        divisorsRaw n x
          | x ^ 2 < n && n `mod` x == 0 = x : n `div` x : divisorsRaw n (x + 1)
          | x ^ 2 < n                   = divisorsRaw n (x + 1)
          | x ^ 2 == n                  = [x]
          | otherwise                   = []