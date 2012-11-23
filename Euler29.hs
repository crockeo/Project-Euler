-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 29
-- Time started: 11/22/2012 - 11:52 PM
-- Time ended: 11/22/2012 - 11:53 PM
-------------------------------

-- Change the N to the current problem
module Euler29 where

import Data.List

e29 = length $ nub [a ^ b | a <- [2..100], b <- [2..100]]

-- Call the main function to get the result
main = e29