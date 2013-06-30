-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 14
-- Time started: 6/27/2013 - ???
-- Time ended: 6/28/2013 - 12:24 PM
-------------------------------

-- Change the N to the current problem
module Euler14 where

greatest :: [(Integer, Integer)] -> (Integer, Integer) -> (Integer, Integer)
greatest [] a = a
greatest (x:xs) a
  | valx > vala = greatest xs x
  | otherwise = greatest xs a
  where valx = snd x
        vala = snd a

evenop :: Integer -> Integer
evenop n = n `div` 2

oddop :: Integer -> Integer
oddop n = (3 * n) + 1

collatzRaw :: Integer -> Integer -> Integer
collatzRaw 1 n = n
collatzRaw x n
  | even x = collatzRaw (evenop x) (n + 1)
  | otherwise = collatzRaw (oddop x) (n + 1)

cap = 999999

-- Call the main function to get the result
main = fst $ greatest [(n, collatzRaw n 0) | n <- [1..cap]] (0, 0)