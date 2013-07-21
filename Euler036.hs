-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 36
-- Time started: 7/20/2013 - 9:54 PM
-- Time ended: 7/20/2013 - 10:07 PM
-------------------------------

-- Change the N to the current problem
module Euler36 where

splitNum :: Integer -> [Integer]
splitNum 0 = []
splitNum n = (splitNum (n `div` 10)) ++ [n `mod` 10]

joinNum :: [Integer] -> Integer
joinNum []     = 0
joinNum (x:xs) = (10 ^ (length xs)) * x + joinNum xs

isPalindrome :: Integer -> Bool
isPalindrome n =
  (sn) == (reverse sn)
  where sn = splitNum n

toBinaryRaw :: Integer -> [Integer]
toBinaryRaw 0 = []
toBinaryRaw n
  | n `mod` 2 /= 0 = 1 : toBinaryRaw (n `div` 2)
  | otherwise      = 0 : toBinaryRaw (n `div` 2)

toBinary :: Integer -> Integer
toBinary n = joinNum $ reverse $ toBinaryRaw n 

cap :: Integer
cap = 1000000

palindromic :: [Integer]
palindromic = [n | n <- [1 .. cap - 1], isPalindrome n, isPalindrome $ toBinary n]

-- Call the main function to get the result
main = sum palindromic