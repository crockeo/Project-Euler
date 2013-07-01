-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 15
-- Time started: 2:06 AM - 6/30/2013
-- Time ended: 12:58 AM - 7/1/2013
-------------------------------

-- Change the N to the current problem
module Euler15 where

fac :: Integer -> Integer
fac 0 = 1
fac n = n * (fac (n - 1))

genPossibilities :: Integer -> Integer
genPossibilities n = (fac (2 * n)) `div` ((fac n) ^ 2)

-- Call the main function to get the result
main = genPossibilities 20