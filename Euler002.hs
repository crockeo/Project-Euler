
-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 2
-- Time started: 11/18/2012 - 12:32 PM
-- Time ended: 11/18/2012 - 12:54 PM
-------------------------------

-- Change the N to the current problem
module Euler2 where

import Utils

-- Call the main function to get the result
main = sum $ filter (\x -> x `mod` 2 == 0) $ takeWhile (\x -> x < 4000000) fibonaccis