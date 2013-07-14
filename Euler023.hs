-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 23
-- Time started: 7/13/2013 - 6:53 PM
-- Time ended:
-------------------------------

-- Change the N to the current problem
module Euler23 where

import Data.List

-- Stripping the copies from a given list
stripCopies :: (Eq a) => [a] -> [a] -> [a]
stripCopies [] prev = []
stripCopies (x:xs) prev
  | elem x prev = stripCopies xs prev
  | otherwise   = x : stripCopies xs (x : prev)

-- The cap for non-abundant-sum numbers
cap :: Int
cap = 28123

-- Finding the divisors of a given number
divisorsRaw :: Int -> Int -> [Int]
divisorsRaw n d
  | (d * d <= n) && (n `mod` d == 0) = d : (n `div` d) : divisorsRaw n (d + 1)
  | (d * d <= n)                     = divisorsRaw n (d + 1)
  | otherwise                        = []

divisors :: Int -> [Int]
divisors n = sort $ stripCopies (divisorsRaw n 1) []

-- Finding the proper divisors (the divisors minus the number itself)
propDivisors :: Int -> [Int]
propDivisors n = init $ divisors n

-- Checking if a number is abundant
isAbundantNumber :: Int -> Bool
isAbundantNumber n = (sum $ propDivisors n) > n

-- A list of abundant numbers
abundantNumbers :: [Int]
abundantNumbers = [n | n <- [1 .. cap], isAbundantNumber n]

-- The sum of abundant numbers
-- TODO: Efficiently generate the sums of abundant numbers
abundantSums :: [Int]
abundantSums = undefined

-- Checking if a number is an abundant sum
isAbundantSum :: Int -> Bool
isAbundantSum n = elem n abundantSums

-- Call the main function to get the result
main = sum [n | n <- [1 .. cap], not $ isAbundantSum n]
