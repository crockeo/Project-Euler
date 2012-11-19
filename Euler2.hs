
-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 2
-- Time started: 11/18/2012 - 12:32 PM
-- Time ended: 11/18/2012 - 12:54 PM
-------------------------------

-- Change the N to the current problem
module Euler2 where

generateFibonacciToLoop a b c array to = do
	if c >= to
		then array
	else (generateFibonacciToLoop b c (b + c) (c : array) to)

generateFibonacciTo to = generateFibonacciToLoop 1 1 2 [1, 1] to

e2 = sum
	(filter
		(\x -> x `mod` 2 == 0)
		(generateFibonacciTo 4000000))

-- Call the main function to get the result
main = print (e2)