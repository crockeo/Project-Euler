-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 17
-- Time started: 6/28/2013 - 1:33 AM
-- Time ended: 6/28/2013 - 3:19 AM
-------------------------------

-- Change the N to the current problem
module Euler17 where

import Data.Maybe

toWord :: Integer -> Maybe String
toWord 1 = Just "one"
toWord 2 = Just "two"
toWord 3 = Just "three"
toWord 4 = Just "four"
toWord 5 = Just "five"
toWord 6 = Just "six"
toWord 7 = Just "seven"
toWord 8 = Just "eight"
toWord 9 = Just "nine"
toWord 10 = Just "ten"
toWord 11 = Just "eleven"
toWord 12 = Just "twelve"
toWord 13 = Just "thirteen"
toWord 14 = Just "fourteen"
toWord 15 = Just "fifteen"
toWord 16 = Just "sixteen"
toWord 17 = Just "seventeen"
toWord 18 = Just "eighteen"
toWord 19 = Just "nineteen"
toWord 20 = Just "twenty"
toWord 30 = Just "thirty"
toWord 40 = Just "forty"
toWord 50 = Just "fifty"
toWord 60 = Just "sixty"
toWord 70 = Just "seventy"
toWord 80 = Just "eighty"
toWord 90 = Just "ninety"
toWord 1000 = Just "one thousand"
toWord n = Nothing

isHundred :: Integer -> Bool
isHundred n = n >= 100 && n < 1000

isTen :: Integer -> Bool
isTen n = n >= 10 && n < 100

isOne :: Integer -> Bool
isOne n = n >= 1 && n < 10
  
makeHundred :: Integer -> Maybe String
makeHundred n
  | res == Nothing = Nothing
  | tensres == Nothing = Just ((fromJust res) ++ " hundred")
  | otherwise = Just ((fromJust res) ++ " hundred and " ++ (fromJust tensres))
  where tens = n `mod` 100
        res = toWord ((n - tens) `div` 100)
        tensres = if isOne tens
                    then makeOne tens
                    else makeTen tens

makeTen :: Integer -> Maybe String
makeTen n
  | res == Nothing = Nothing
  | n >= 10 && n < 20 = toWord n
  | onesres == Nothing = res
  | otherwise = Just ((fromJust res) ++ "-" ++ (fromJust onesres))
  where ones = n `mod` 10
        res = toWord (n - ones)
        onesres = makeOne ones

makeOne :: Integer -> Maybe String
makeOne n = toWord n

makeString :: Integer -> Maybe String
makeString n
  | n == 1000 = toWord n
  | isHundred n = makeHundred n
  | isTen n = makeTen n
  | isOne n = makeOne n
  | otherwise = Nothing

stripSpace :: String -> String
stripSpace [] = []
stripSpace (x:xs)
  | x == '-' || x == ' ' = stripSpace xs
  | otherwise = x : (stripSpace xs)

calcrange end = sum $ map (\x -> length $ stripSpace x) [fromJust $ makeString n | n <- [1..1000]]

-- Call the main function to get the result
main = calcrange 1000