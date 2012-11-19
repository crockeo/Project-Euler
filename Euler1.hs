
-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 1
-- Time started: 11/18/2012 - ~3:20 AM
-- Time ended: 11/18/2012 - 3:26 AM
-------------------------------

-- Change the N to the current problem
module Euler1 where

-- Finding the 
e1 = sum
	(filter
		(\x -> x `mod` 3 == 0 || x `mod` 5 == 0)
		(map (+0) [3..999]))

-- Call the main function to get the result
main = print (e1)