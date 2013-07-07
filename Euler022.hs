-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 22
-- Time started: 7/6/2013 - 10:10 PM
-- Time ended: 7/7/2013 - 12:25 AM
-------------------------------

-- Change the N to the current problem
module Euler22 where

import Control.Applicative

import System.IO.Unsafe

import System.Directory
import System.IO

import Data.Maybe
import Data.Char
import Data.List

toNum :: Char -> Maybe Int
toNum c = (+1) <$> (elemIndex (toLower c) ['a' .. 'z'])

stringScore :: String -> Maybe Int
stringScore [] = Just 0
stringScore (x:xs) = (+) <$> (toNum x) <*> (stringScore xs)

nameScore :: Int -> [String] -> Maybe Int
nameScore n str = (* (n + 1)) <$> (stringScore (str !! n))

allNameScores :: [String] -> [Maybe Int]
allNameScores strs = [nameScore n strs | n <- [0..((length strs) - 1)]]

fixNameScores :: [Maybe Int] -> [Int]
fixNameScores [] = []
fixNameScores (x:xs)
  | x == Nothing = fixNameScores xs
  | otherwise = (fromJust x) : (fixNameScores xs)

fileContents :: IO [String]
fileContents = do
  contents <- readFile "Euler022Data/names.txt"
  return $ sort $ read ("[" ++ contents ++ "]" )

-- Call the main function to get the result
main = do
  contents <- fileContents
  return (sum $ fixNameScores $ allNameScores contents)