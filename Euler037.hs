-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 37
-- Time started: 7/21/2013 - 4:00 PM
-- Time ended: 7/21/2013 - 8:13 PM
-------------------------------

-- Change the N to the current problem
module Euler37 where

import Utils

takeFromLeft :: Int -> Int
takeFromLeft n = joinNum $ tail $ splitNum n

takeFromRight :: Int -> Int
takeFromRight n = joinNum $ init $ splitNum n

genTruncLeft :: Int -> [Int]
genTruncLeft 0 = []
genTruncLeft n = n : (genTruncLeft $ takeFromLeft n)

genTruncRight :: Int -> [Int]
genTruncRight 0 = []
genTruncRight n = n : (genTruncRight $ takeFromRight n)

allPrime :: [Int] -> Bool
allPrime list = and $ map (isPrime) list

isNum :: Int -> Bool
isNum n = (isPrime n) && (allPrime $ tail $ genTruncLeft n) && (allPrime $ tail $ genTruncRight n)

-- Call the main function to get the result
main = sum $ take 11 [n | n <- [10 ..], isNum n]