-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 34
-- Time started: 7/20/2013 - 6:01 PM
-- Time ended: 7/20/2013 - 6:08 PM
-------------------------------

-- Change the N to the current problem
module Euler34 where

import Utils

digitFactorials :: Int -> Int
digitFactorials n = sum $ map (\x -> factorials !! x) $ splitNum n

nums :: [Int]
nums = [n | n <- [1 ..], digitFactorials n == n]

-- Call the main function to get the result
main = sum $ drop 2 $ take 4 nums