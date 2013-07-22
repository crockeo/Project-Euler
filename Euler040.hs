-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 40
-- Time started: 7/21/2013 - 12:12 AM
-- Time ended: 7/21/2013 - 12:27 AM
-------------------------------

-- Change the N to the current problem
module Euler40 where

import Data.List
import Utils

list :: [Int]
list = concat [splitNum n | n <- [1 ..]]

-- Call the main function to get the result
main = (list !! 0) * (list !! 9) * (list !! 99) * (list !! 999) * (list !! 9999) * (list !! 99999) * (list !! 999999) 