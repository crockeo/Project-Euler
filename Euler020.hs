-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 20
-- Time started: 7/6/2013 - 9:17 AM
-- Time ended: 7/6/2013 - 9:32 AM
-------------------------------

-- Change the N to the current problem
module Euler20 where

-- Sum of digits of 100!

fac :: Integer -> Integer
fac 0 = 1
fac n = n * (fac (n - 1))

splitnum :: Integer -> [Integer]
splitnum 0 = []
splitnum n = (splitnum (n `div` 10)) ++ [n `mod` 10]

-- Call the main function to get the result
main = sum $ splitnum $ fac 100