-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 24
-- Time started: 7/12/2013 - 3:47 PM
-- Time ended: 7/13/2013 - 12:54 PM
-------------------------------

-- Change the N to the current problem
module Euler24 where

import Data.List

permloc :: Int
permloc = 999999

lexPerms :: [Int] -> [[Int]]
lexPerms list = sort $ permutations list

-- Call the main function to get the result
main = lexPerms [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] !! permloc