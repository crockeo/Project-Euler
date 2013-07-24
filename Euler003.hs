-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 3
-- Time started: 11/18/2012 - 2:40 PM
-- Time ended:
-------------------------------

-- Change the N to the current problem
module Euler3 where

import Utils

number :: Integer
number = 600851475143

-- Call the main function to get the result
main = last [x | x <- [2 .. cleanSqrt number], number `mod` x == 0, isPrime x]