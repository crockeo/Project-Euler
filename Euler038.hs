-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 38
-- Time started: 7/21/2013 - 9:49 PM
-- Time ended: 7/22/2013 - 3:11 PM
-------------------------------

-- Change the N to the current problem
module Euler38 where

import Utils

makeNumberRaw :: Int -> Int -> Int -> [Int] -> Int
makeNumberRaw panl bn sn list
  | length list == panl = joinNum list
  | length concated > panl = 0
  | otherwise = makeNumberRaw panl bn (sn + 1) concated
  where mul = bn * sn
        smul = splitNum mul
        concated = list ++ smul

makeNumber :: Int -> Int -> Int
makeNumber panl bn = makeNumberRaw panl bn 1 []

absCap :: Int
absCap = 999999999

genCap :: Int
genCap = (head [n | n <- [1 .. absCap], (length $ splitNum n) * 2 > (length $ splitNum absCap)]) - 1

list :: [Int]
list = filter (/= 0) [makeNumber 9 n | n <- [1 .. genCap], isPandigital $ makeNumber 9 n]

-- Call the main function to get the result
main = maximum list