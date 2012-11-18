
-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 1
-- Time started: 11/18/2012 - ~3:20 AM
-- Time ended: 11/18/2012 - 3:26 AM
-------------------------------

-- Change the N to the current problem
module Euler1 where

e1loop n num = do
	if n >= 1000
		then num
	else
		if (n `mod` 3 == 0) || (n `mod` 5 == 0)
			then (e1loop (n + 1) (num + n))
		else
			(e1loop (n + 1) num)

-- Call the main function to get the result
main = print (e1loop 3 0)