-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 6
-- Time started: 11/19/2012 - 6:01 AM
-- Time ended: 11/19/2012 - 6:05 AM
-------------------------------

-- Change the N to the current problem
module Euler6 where

sumofsquares = sum [x ** 2 | x <- [1..100]]
squareofsum = (sum [x | x <- [1..100]]) ** 2

e6 = squareofsum - sumofsquares

-- Call the main function to get the result
main = e6