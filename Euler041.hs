-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 41
-- Time started: 9/23/2013 - 12:00 PM
-- Time ended: 9/23/2013 - 12:30 PM
-------------------------------

-- Change the N to the current problem
module Euler41 where

import Utils

panPrimes :: [Integer]
panPrimes = filter (isPandigital) primes

num :: Integer
num = 7652413 -- Found via watching the panPrimes function
              -- not produce any more integers

-- Call the main function to get the result
main = num