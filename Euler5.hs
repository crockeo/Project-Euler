-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 5
-- Time started: 11/19/2012 - 5:32 AM
-- Time ended: 11/19/2012 - 5:58 AM
-------------------------------

-- Change the N to the current problem
module Euler5 where

e5 = take 1 [x | x <- [1..], x `mod` 2 == 0, x `mod` 3 == 0, x `mod` 4 == 0, x `mod` 5 == 0, x `mod` 6 == 0, x `mod` 7 == 0, x `mod` 8 == 0, x `mod` 9 == 0, x `mod` 10 == 0, x `mod` 11 == 0, x `mod` 12 == 0, x `mod` 13 == 0, x `mod` 14 == 0, x `mod` 15 == 0, x `mod` 16 == 0, x `mod` 17 == 0, x `mod` 18 == 0, x `mod` 19 == 0, x `mod` 20 == 0]

-- Call the main function to get the result
main = e5