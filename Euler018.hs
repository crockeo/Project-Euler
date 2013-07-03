-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 18
-- Time started: 7/1/2013 - 4:20 PM
-- Time ended: 7/3/2013 - 12:33 AM
-------------------------------

-- Change the N to the current problem
module Euler18 where

import Control.Applicative
import Control.Monad

import Data.Maybe

type Point = (Int, Int)

translate :: Point -> Point -> Point
translate (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

data Direction = GoLeft | GoRight
  deriving (Eq, Show, Read)

toPoint :: Direction -> Point
toPoint GoLeft = (0, 1)
toPoint GoRight = (1, 1)

moveDirection :: Direction -> Point -> Point
moveDirection dir pos = translate (toPoint dir) pos

type Triangle = [[Int]]
type Path = [Direction]

triangleHeight :: Triangle -> Int
triangleHeight tri = length tri

triangleWidth :: Triangle -> Int -> Int
triangleWidth tri row = length $ tri !! row

getElem :: Triangle -> Point -> Maybe Int
getElem triangle (x, y)
  | y < (triangleHeight triangle) && x < (triangleWidth triangle y) = Just ((triangle !! y) !! x)
  | otherwise = Nothing

traverseTriangleRaw :: Triangle -> Path -> Point -> Int
traverseTriangleRaw triangle [] pos
  | currelem == Nothing = 0
  | otherwise = fromJust currelem
  where currelem = getElem triangle pos

traverseTriangleRaw triangle (x:xs) pos
  | currelem == Nothing = 0
  | otherwise = (fromJust currelem) + (traverseTriangleRaw triangle xs (moveDirection x pos))
  where currelem = getElem triangle pos

traverseTriangle :: Triangle -> Path -> Int
traverseTriangle triangle path = traverseTriangleRaw triangle path (0, 0) 

traverseTriangleAll :: Triangle -> [Path] -> [Int]
traverseTriangleAll triangle paths = [traverseTriangle triangle x | x <- paths]

genPaths :: Triangle -> [Path]
genPaths triangle = replicateM ((triangleHeight triangle) - 1) [GoLeft, GoRight] 

greatest :: [Int] -> Int
greatest [] = 0
greatest (x:xs)
  | x > next = x
  | otherwise = next
  where next = greatest xs

findGreatestPath :: Triangle -> Int
findGreatestPath triangle = greatest $ traverseTriangleAll triangle $ genPaths triangle

problemTriangle :: Triangle
problemTriangle = [[75], [95, 64], [17, 47, 82], [18, 35, 87, 10], [20, 04, 82, 47, 65], [19, 01, 23, 75, 03, 34], [88, 02, 77, 73, 07, 63, 67], [99, 65, 04, 28, 06, 16, 70, 92], [41, 41, 26, 56, 83, 40, 80, 70, 33], [41, 48, 72, 33, 47, 32, 37, 16, 94, 29], [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14], [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57], [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48], [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31], [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

-- Call the main function to get the result
main = findGreatestPath problemTriangle