-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 28
-- Time started: 7/17/2013 - 4:04 PM
-- Time ended: 7/17/2013 - 7:41 PM
-------------------------------

-- Change the N to the current problem
module Euler28 where

upperLimit :: Int
upperLimit = 1000000

takeRange :: Int -> Int -> [a] -> [a]
takeRange f t list
  | f < 0 || t >= (length list) = []
  | otherwise = range
  where llength = length list
        range = take t $ drop f list

ul :: [Int]
ul = 1 : [6 + (8 * (n - 1)) + (ul !! (n - 1)) | n <- [1 .. upperLimit]]

ur :: [Int]
ur = 1 : [8 + (8 * (n - 1)) + (ur !! (n - 1)) | n <- [1 .. upperLimit]]

dl :: [Int]
dl = 1 : [2 + (8 * (n - 1)) + (dl !! (n - 1)) | n <- [1 .. upperLimit]]

dr :: [Int]
dr = 1 : [4 + (8 * (n - 1)) + (dr !! (n - 1)) | n <- [1 .. upperLimit]]

ulsum :: Int -> Int
ulsum n = sum $ takeRange 1 n ul

ursum :: Int -> Int
ursum n = sum $ takeRange 1 n ur

dlsum :: Int -> Int
dlsum n = sum $ takeRange 1 n dl

drsum :: Int -> Int
drsum n = sum $ takeRange 1 n dr

-- Call the main function to get the result
main =
  1 + ulsum n + ursum n + dlsum n + drsum n
  where size = 1001
        n = size `div` 2