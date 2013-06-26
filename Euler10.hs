-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 10
-- Time started: 6/25/2013 - 10:31 PM
-- Time ended: 6/26/2013 - 12:35 AM
-------------------------------

-- Change the N to the current problem
module Euler10 where

anyTrue :: [Bool] -> Bool
anyTrue [] = False
anyTrue (x:xs) = x || anyTrue xs

isPrimeLoop :: Integer -> Integer -> Bool
isPrimeLoop n x
  | x * x <= n = (n `mod` x == 0) || (isPrimeLoop n (x + 1))
  | otherwise = False

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = not $ isPrimeLoop n 2

nums = [n | n <- [1..2000000], isPrime n]

-- Call the main function to get the result
main = sum nums