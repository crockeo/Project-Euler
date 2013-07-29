-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 42
-- Time started: 7/28/2013 - 10:22 PM
-- Time ended: 7/28/2013 - 10:39 PM
-------------------------------

-- Change the N to the current problem
module Euler42 where

import Data.Char

import Utils

triangles :: [Int]
triangles = [n * (n + 1) `div` 2 | n <- [1 ..]]

isTriangle :: Int -> Bool
isTriangle n = quickElem n triangles

charValue :: Char -> Int
charValue c = (ord c) - 96

stringValue :: String -> Int
stringValue s = sum $ map (charValue) s

isTriangleWord :: String -> Bool
isTriangleWord = isTriangle . stringValue

location :: String
location = "Euler042Data/words.txt"

iows :: IO [String]
iows = do
  w <- readFile location
  return $ read $ map (toLower) ("[" ++ w ++ "]")

triangleWords :: IO [String]
triangleWords = do
  ws <- iows
  return [s | s <- ws, isTriangleWord s]

-- Call the main function to get the result
main = do
  ws <- triangleWords
  return $ length ws
