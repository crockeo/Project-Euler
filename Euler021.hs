-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 21
-- Time started: 7/6/2013 - 9:34 AM
-- Time ended: 
------------------------------- 

-- Find amicable pairs

-- Change the N to the current problem
module Euler21 where

import Data.List

cleanRaw :: (Eq a) => [a] -> [a] -> [a]
cleanRaw [] prev = []
cleanRaw (x:xs) prev =
  if x `elem` prev
    then cleanRaw xs prev
    else x : (cleanRaw xs (x : prev))

clean :: (Eq a) => [a] -> [a]
clean list = cleanRaw list []

divisible :: Int -> Int -> Bool
divisible n1 n2 = n1 `mod` n2 == 0

findDivisorsRaw :: Int -> Int -> [Int]
findDivisorsRaw n x
  | ((x ^ 2) <= n) && (n `mod` x == 0) = x : (n `div` x) : findDivisorsRaw n (x + 1)
  | ((x ^ 2) <= n) = findDivisorsRaw n (x + 1)
  | otherwise = []

findDivisors :: Int -> [Int]
findDivisors n =
  take ((length divisors) - 1) divisors
  where divisors = sort $ clean $ findDivisorsRaw n 1

divisorsSum :: Int -> Int
divisorsSum n = sum $ findDivisors n

isAmicableNumber :: Int -> Bool
isAmicableNumber n =
  (divs1 /= n) && (divs2 == n)
  where divs1 = divisorsSum n
        divs2 = divisorsSum divs1

findAmicableNumbers :: [Int]
findAmicableNumbers = [n | n <- [1..10000], isAmicableNumber n]

-- Call the main function to get the result
main = sum findAmicableNumbers