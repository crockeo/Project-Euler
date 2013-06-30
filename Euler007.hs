-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 7
-- Time started: 11/19/2012 - 6:07 AM
-- Time ended: 11/19/2012 - 6:09 AM
-------------------------------

-- Change the N to the current problem
module Euler7 where

isPrimeLoop curr n = do
	if (n == 1)
		then False
	else if (n == 2)
		then True
	else
		if ((curr * curr) > n)
			then True
		else
			if (n `mod` curr == 0)
				then False
			else (isPrimeLoop (curr + 1) n)

isPrime n = (isPrimeLoop 2 n)

e7 = last (take 10001 [x | x <- [1..], isPrime x])
	
-- Call the main function to get the result
main = e7