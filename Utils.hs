-----------------------------------
-- A file containing a number of --
-- functions I've found helpful  --
-- and reusable in Project Euler --
-----------------------------------
-- Author: Cerek Hillen --
--------------------------

module Utils where

-- Checking if a number is prime
isPrimeRaw :: Int -> Int -> Bool
isPrimeRaw n x
  | x * x > n = True
  | n `mod` x == 0 = False
  | otherwise = isPrimeRaw n (x + 1)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = isPrimeRaw n 2

-- Splitting and joining integers
splitNumRaw :: (Integral a) => a -> [a]
splitNumRaw 0 = []
splitNumRaw n = (n `mod` 10) : splitNumRaw (n `div` 10)

splitNum :: (Integral a) => a -> [a]
splitNum n = reverse $ splitNumRaw n

joinNum :: (Integral a) => [a] -> a
joinNum [] = 0
joinNum (x:xs) = (10 ^ (length xs)) * x + joinNum xs

-- A list of factorials
factorials :: [Integer]
factorials = 1 : [n * factorials !! ((fromInteger n :: Int) - 1) | n <- [1 ..]]

-- A list of fibonacci terms
fibonaccis :: [Integer]
fibonaccis = 0 : 1 : [fibonaccis !! ((fromInteger n :: Int) - 1) + fibonaccis !! ((fromInteger n :: Int) - 2) | n <- [2 ..]]

-- A list of primes
primes :: [Int]
primes = [n | n <- [2 ..], isPrime n]

-- Search functions
quickElem :: (Ord a, Eq a) => a -> [a] -> Bool
quickElem n [] = False
quickElem n (x:xs)
  | n <  x    = False
  | n == x    = True
  | otherwise = quickElem n xs