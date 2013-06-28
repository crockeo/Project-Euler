-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 16
-- Time started: 6/28/2013 - 1:30 AM
-- Time ended: 6/28/2013 - 1:31 AM
-------------------------------

-- Change the N to the current problem
module Euler16 where

splitNum :: Integer -> [Integer]
splitNum 0 = []
splitNum num = (num `mod` 10) : (splitNum (num `div` 10))

-- Call the main function to get the result
main = sum $ splitNum (2 ^ 1000)