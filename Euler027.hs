-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 27
-- Time started: 7/17/2013 - 10:40 PM
-- Time ended: 7/18/2013 - 12:25 AM
-------------------------------

-- Change the N to the current problem
module Euler27 where

isPrimeRaw :: Int -> Int -> Bool
isPrimeRaw n x
  | n < 0          = False
  | x * x > n      = True
  | n `mod` x == 0 = False
  | otherwise      = isPrimeRaw n (x + 1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = isPrimeRaw n 2

doQuadratic :: Int -> Int -> Int -> Int
doQuadratic n a b = (n ^ 2) + (a * n) + (b)

isQuadraticPrime :: Int -> Int -> Int -> Bool
isQuadraticPrime n a b = isPrime $ doQuadratic n a b

generatesPrimes :: Int -> Int -> Int
generatesPrimes a b = length $ takeWhile (== True) [isQuadraticPrime n a b | n <- [0 ..]]

generatesPrimesList :: [((Int, Int), Int)]
generatesPrimesList = [((a, b), generatesPrimes a b) | a <- [-999 .. 999], b <- [-999 .. 999]]

pMax :: [((Int, Int), Int)] -> ((Int, Int), Int)
pMax [] = ((0, 0), 0)
pMax (x:xs) = 
  if p1 > p2
    then x
    else next
  where next = pMax xs
        p1   = snd x
        p2   = snd next

-- Call the main function to get the result
main = a * b
  where pmax = pMax generatesPrimesList
        a = fst $ fst pmax
        b = snd $ fst pmax