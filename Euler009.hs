-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 9
-- Time started: 11/20/2012 - 2:08 PM
-- Time ended: 11/21/2012 - 3:15 AM
-------------------------------

-- Change the N to the current problem
module Euler9 where

e9 = head [a * b * c | c <- [3..998], b <- [2..(c - 1)], a <- [1..(b - 1)], a + b + c == 1000, (a * a) + (b * b) == (c * c)]
	
-- Call the main function to get the result
main = e9