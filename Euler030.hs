-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 30
-- Time started: 7/17/2013 - 6:40 PM
-- Time ended: 7/17/2013 - 10:22 PM
-------------------------------

-- Change the N to the current problem
module Euler30 where

splitNumRaw :: Int -> [Int]
splitNumRaw 0 = []
splitNumRaw n = (n `mod` 10) : splitNumRaw (n `div` 10)

splitNum :: Int -> [Int]
splitNum n = reverse $ splitNumRaw n

powerEach :: Int -> [Int] -> [Int]
powerEach n list = map (\x -> x ^ n) list

madeFromPoweredDigits :: Int -> Int -> Bool
madeFromPoweredDigits n x = (sum $ powerEach x $ splitNum n) == n

madeFromPowerFive :: [Int]
madeFromPowerFive = [n | n <- [2 ..], madeFromPoweredDigits n 5]

-- Call the main function to get the result
main = sum $ take 6 madeFrimPowerFive