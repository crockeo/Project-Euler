-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 3
-- Time started: 11/18/2012 - 2:40 PM
-- Time ended:
-------------------------------

-- Change the N to the current problem
module Euler3 where

number = 600851475143

-- Determining if a number is prime
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

e3 :: [Integer]
e3 = [x | x <- [1 .. (sqrt number)], number `mod` x == 0, isPrime x]

-- Call the main function to get the result
main = print (e3)