-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 35
-- Time started: 7/20/2013 - 6:09 PM
-- Time ended: 7/20/2013 - 9:32 PM
-------------------------------

-- Change the N to the current problem
module Euler35 where

import Data.List

isPrimeRaw :: Int -> Int -> Bool
isPrimeRaw n x
  | x * x > n      = True
  | n `mod` x == 0 = False
  | otherwise      = isPrimeRaw n (x + 1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = isPrimeRaw n 2

-- Rotating a list to the right
rotateList :: [a] -> [a]
rotateList [] = []
rotateList list = (last list) : (init list)

-- Splitting and joining numbers
splitNum :: Int -> [Int]
splitNum 0 = []
splitNum n = (splitNum (n `div` 10)) ++ [n `mod` 10]

joinNum :: [Int] -> Int
joinNum [] = 0
joinNum (x:xs) = (10 ^ (length xs)) * x + joinNum xs

-- Creating the rotations of a number
createRotationsRaw :: [Int] -> [[Int]] -> [[Int]]
createRotationsRaw n prev
  | elem n prev = prev
  | otherwise = createRotationsRaw (rotateList n) (n : prev)

createRotations :: Int -> [Int]
createRotations n = map (joinNum) $ createRotationsRaw (splitNum n) []

-- Checking if a number is a circular prime
isCircularPrime :: Int -> Bool
isCircularPrime n = (isPrime n) && (and $ map (\x -> isPrime x) $ createRotations n) 

cap :: Int
cap = 1000000

-- Generating circular primes
generateCircularPrimesRaw :: Int -> [Int] -> [Int]
generateCircularPrimesRaw n list
  | n >= cap     = list
  | elem n list  = generateCircularPrimesRaw (n + 1) list
  | and allprime = generateCircularPrimesRaw (n + 1) (circs ++ list)
  | otherwise    = generateCircularPrimesRaw (n + 1) list
  where circs = createRotations n
        allprime = map (isPrime) circs

generateCircularPrimes :: [Int]
generateCircularPrimes = generateCircularPrimesRaw 1 []

circularPrimes :: [Int]
circularPrimes = [n | n <- [1 .. cap - 1], isCircularPrime n]

-- Call the main function to get the result
main = length [n | n <- [1 .. cap - 1], isCircularPrime n]