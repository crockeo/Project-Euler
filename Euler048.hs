-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 48
-- Time started: 4/30/2014 - 12:20 PM
-- Time ended: 4/30/2014 - 12:28 PM
-------------------------------

-- Change the N to the current problem
module Euler48 where

import Utils

-- Calculating the sums of a self-power
calcN :: (Integral a) => a -> a
calcN n =
  sum [x ^ x | x <- [1 .. n]]
          
-- Call the main function to get the result
main =
  joinNumBackwards $ take 10 $ splitNumBackwards $ calcN 1000