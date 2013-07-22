-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 33
-- Time started: 7/21/2013 - 8:20 PM
-- Time ended: 7/21/2013 - 9:16 PM
-------------------------------

-- Change the N to the current problem
module Euler33 where

import Utils

type Fraction = (Int, Int)

-- Converting an int to a float
toFloat :: Int -> Float
toFloat n = fromInteger (toInteger n) :: Float

-- Stripping digits from either the left or the right
takeFromLeft :: Int -> Int
takeFromLeft n = joinNum $ tail $ splitNum n

takeFromRight :: Int -> Int
takeFromRight n = joinNum $ init $ splitNum n

-- Checking if a fraction is in the proper format
properFormat :: Fraction -> Bool
properFormat (a, b) =
  ((sa !! 0) == (sb !! 1)) || ((sa !! 1) == (sb !! 0))
  where sa = splitNum a
        sb = splitNum b

-- Checking if the custom equality test is true
areEqual :: Fraction -> Bool
areEqual (a, b)
  | anyzeroes = False
  | otherwise = (fa / fb == ffla / ffrb) || (fa / fb == ffra / fflb)
  where anyzeroes = (takeFromLeft a == 0) || (takeFromRight a == 0) || (takeFromLeft b == 0) || (takeFromRight b == 0)
        fa = toFloat a
        fb = toFloat b
        ffla = toFloat $ takeFromLeft a
        fflb = toFloat $ takeFromLeft b
        ffra = toFloat $ takeFromRight a
        ffrb = toFloat $ takeFromRight b

-- Checking if it's a number pair
isNumPair :: Fraction -> Bool
isNumPair (a, b) = (properFormat (a, b)) && (areEqual (a, b))

-- A list of generated pairs
pairs :: [Fraction]
pairs = [(a, b) | a <- [10 .. 99], b <- [(a + 1) .. 99], isNumPair (a, b)]

-- Multiplying two fractions
mul :: Fraction -> Fraction -> Fraction
mul (a1, b1) (a2, b2) = (a1 * a2, b1 * b2)

-- Multiplying all fractions in a list
mulAll :: [Fraction] -> Fraction
mulAll [] = (1, 1)
mulAll (x:xs) = mul x $ mulAll xs

-- Simplifying a fraction
simplifyRaw :: Fraction -> Int -> Fraction
simplifyRaw f@(a, b) n
  | n * n > b                            = f
  | (a `mod` n == 0) && (b `mod` n == 0) = (a `div` n, b `div` n)
  | otherwise                            = simplifyRaw f (n + 1)

simplify :: Fraction -> Fraction
simplify f
  | f == simped = f
  | otherwise = simplify simped
  where simped = simplifyRaw f 2

-- Call the main function to get the result
main = snd $ simplify $ mulAll pairs