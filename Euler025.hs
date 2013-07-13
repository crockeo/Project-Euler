-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 25
-- Time started: 7/12/2013 - 3:58 PM
-- Time ended: 7/13/2013 - 12:54 PM
-------------------------------

-- Change the N to the current problem
module Euler25 where

import Data.Maybe
import Data.List

fibs :: [Integer]
fibs = 0 : 1 : [(fibs !! (n - 1)) + (fibs !! (n - 2)) | n <- [2..]]

numlength :: Integer -> Int
numlength 0 = 0
numlength x = 1 + (numlength (x `div` 10))

-- Call the main function to get the result
main =
  if index == Nothing
    then (-1)
    else fromJust index
  where index = elemIndex (head $ take 1 $ filter (\x -> numlength x >= 1000) fibs) fibs