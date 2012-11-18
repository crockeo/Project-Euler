
-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 2
-- Time started: 11/18/2012 - 12:32 PM
-- Time ended: 11/18/2012 - 12:54 PM
-------------------------------

-- Change the N to the current problem
module Euler2 where

e2loop a b c num = do
	if c > 4000000
		then num
	else
		if c `mod` 2 == 0
			then (e2loop b c (b + c) (num + c))
		else
			(e2loop b c (b + c) num)

-- Call the main function to get the result
main = print (e2loop 1 1 2 0)