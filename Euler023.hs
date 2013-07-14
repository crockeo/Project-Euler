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

elemQuick :: (Eq a, Ord a) => a -> [a] -> Bool
elemQuick n [] = False
elemQuick n (x:xs)
  | n < x     = False
  | n == x    = True
  | otherwise = elemQuick n xs

anyCopies :: (Eq a) => [a] -> [a] -> Bool
anyCopies [] prev = False
anyCopies (x:xs) prev
  | elem x prev = True
  | otherwise   = anyCopies xs (x : prev)

-- The cap for non-abundant-sum numbers
cap :: Int
cap = 20161

-- Finding the divisors of a given number
divisorsRaw :: Int -> Int -> [Int]
divisorsRaw n d
  | (d * d <= n) && (n `mod` d == 0) = d : (n `div` d) : divisorsRaw n (d + 1)
  | (d * d <= n)                     = divisorsRaw n (d + 1)
  | otherwise                        = []

divisors :: Int -> [Int]
divisors n = sort $ nub $ divisorsRaw n 1

-- Finding the proper divisors (the divisors minus the number itself)
propDivisors :: Int -> [Int]
propDivisors n = init $ divisors n

-- Checking if a number is abundant
isAbundantNumber :: Int -> Bool
isAbundantNumber n = (sum $ propDivisors n) > n

-- Checking if a tuple of two integers are both abundant
areBothAbundant :: (Int, Int) -> Bool
areBothAbundant (n1, n2) = isAbundantNumber n1 && isAbundantNumber n2

-- Get addses
getAddses :: Int -> [(Int, Int)]
getAddses n = zip [1 .. n `div` 2] [n - 1, n - 2 .. n `div` 2] 

-- A list of abundant numbers
abundantNumbers :: [Int]
abundantNumbers = [n | n <- [1 .. cap], isAbundantNumber n]

-- The sum of abundant numbers
abundantSums :: [Int]
abundantSums = [(abundantNumbers !! x) + (abundantNumbers !! y) | x <- [0 .. (length abundantNumbers) - 1], y <- [x .. (length abundantNumbers) - 1]]

-- Checking if a number is an abundant sum
isAbundantSum :: Int -> Bool
isAbundantSum n
  | (n > 48) && (n `mod` 2 == 0) = True
  | otherwise                    = (filter (areBothAbundant) $ getAddses n) /= []

-- Call the main function to get the result
main = sum [n | n <- [1 .. cap], not $ isAbundantSum n]
