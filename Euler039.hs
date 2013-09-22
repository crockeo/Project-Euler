-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 39
-- Time started: 9/19/2013 - 3:00 PM
-- Time ended: 9/20/2013 - 1:25 PM
-------------------------------

-- Change the N to the current problem
module Euler39 where

import Data.List
import Utils

------------------------------------
-- Setting an element in an array --
------------------------------------
setElem :: [a] -> a -> Int -> [a]
setElem l e n =
  [if x == n then e else l !! x | x <- [0 .. (length l) - 1]]

-------------------------------------------------------------
-- Determining the frequency of an element in a given list --
-------------------------------------------------------------
frequency :: (Eq a) => [a] -> [(a, Int)]
frequency l =
  frequencyRaw l []
  where frequencyRaw :: (Eq a) => [a] -> [(a, Int)] -> [(a, Int)]
        frequencyRaw [] l = l
        frequencyRaw (x:xs) l =
          case elemIndex x $ map (fst) l of
            (Just n) -> frequencyRaw xs $ setElem l (inc $ l !! n) n
            Nothing  -> frequencyRaw xs ((x, 1) : l)
          where inc :: (a, Int) -> (a, Int)
                inc (a, n) = (a, n + 1)

sortedFrequency :: (Eq a, Ord a) => [a] -> [(a, Int)]
sortedFrequency = sortBy (\a b -> compare (snd a) (snd b)) . frequency

----------------------------------
-- Getting the sum of a triplet --
----------------------------------
tripSum :: (Int, Int, Int) -> Int
tripSum (a, b, c) = a + b + c

-----------------------------------------
-- Generating all pythagorean triplets --
-----------------------------------------
generateTriplets :: [(Int, Int, Int)]
generateTriplets = [(a, b, cleanSqrt (a^2 + b^2)) | b <- [4 .. 500], a <- [3 .. b], a^2 + b^2 == cleanSqrt (a^2 + b^2)^2]

-- Call the main function to get the result
main = last $ sortBy (\a b -> compare (snd a) (snd b)) $ frequency $ map (tripSum) generateTriplets