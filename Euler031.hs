-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 31
-- Time started: 9/19/2013 - 10:25 AM
-- Time ended: 9/19/2013 - 10:34 AM
-------------------------------

-- Change the N to the current problem
module Euler31 where

makesMoney :: Int -> [Int] -> Int
makesMoney 0 l = 1
makesMoney n [] = 0
makesMoney n l@(x:xs)
  | n < 0 = 0
  | otherwise = makesMoney n xs + makesMoney (n - x) l

-- Call the main function to get the result
main =
  makesMoney target coins
  where target = 200
        coins  = [200, 100, 50, 20, 10, 5, 2, 1]