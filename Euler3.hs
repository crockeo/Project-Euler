-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 3
-- Time started: 11/18/2012 - 2:40 PM
-- Time ended:
-------------------------------

-- Change the N to the current problem
module Euler3 where

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

getPrimeFactorsLoop current number factors = do
	if ((current * current) > number)
		then True
	else
		if ((number `mod` current == 0) && (isPrime number))
			then (getPrimeFactorsLoop 1 (number / current) (current : factors))
		else (getPrimeFactorsLoop (current + 2) number factors)


getPrimeFactors number = getPrimeFactorsLoop 1 number [0]

-- Call the main function to get the result
main = getPrimeFactors 600851475143