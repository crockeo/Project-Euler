-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 12
-- Time started: 6/27/2013 - 5:49 PM
-- Time ended: 6/27/2013 - 9:54 PM
-------------------------------

-- Change the N to the current problem
module Euler12 where

eliminateRepeats :: (Eq a) => [a] -> [a]
eliminateRepeats [] = []
eliminateRepeats (x:xs)
  | elem x xs = eliminateRepeats xs
  | otherwise = x : (eliminateRepeats xs)

genTriangleNumber :: Integer -> Integer
genTriangleNumber n = (n * (n + 1)) `div` 2

determineDivisorsRaw :: Integer -> Integer -> [Integer]
determineDivisorsRaw num n
  | (n * n) <= num =
    if divisible
      then n : (num `div` n) : determineDivisorsRaw num (n + 1)
      else determineDivisorsRaw num (n + 1)
  | otherwise = []
  where divisible = num `mod` n == 0

determineDivisors :: Integer -> [Integer]
determineDivisors num = eliminateRepeats $ determineDivisorsRaw num 1

-- Call the main function to get the result
main = head [genTriangleNumber n | n <- [1..], (length $ determineDivisors $ genTriangleNumber n) > 500]