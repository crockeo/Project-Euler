-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 15
-- Time started: 2:06 AM - 6/30/2013
-- Time ended:
-------------------------------

-- Change the N to the current problem
module Euler15 where

import Control.Monad
import Data.List

fac 0 = 1
fac n = n * fac (n - 1)

data Direction = GoRight | GoDown
  deriving (Eq, Show, Read)
type Point = (Integer, Integer)

translate :: Point -> Point -> Point
translate (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

toPoint :: Direction -> Point
toPoint GoRight = (1, 0)
toPoint GoDown = (0, 1)

moveDirection :: Direction -> Point -> Point
moveDirection dir point = translate point $ toPoint dir

reachesEndRaw :: Point -> [Direction] -> Point -> Bool
reachesEndRaw point [] end = point == end
reachesEndRaw point (x:xs) end = reachesEndRaw (moveDirection x point) xs end

reachesEnd :: [Direction] -> Point -> Bool
reachesEnd dirs end = reachesEndRaw (0, 0) dirs end

genList :: Point -> [[Direction]]
genList size@(w, h) = filter (\x -> reachesEnd x size) (replicateM (fromIntegral (w + h) :: Int) [GoRight, GoDown])

-- Call the main function to get the result
main = length $ genList (20, 20)