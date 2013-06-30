-------------------------------
-- Project Euler Challenge File
-------------------------------
-- Number: 4
-- Time started: 11/19/2012 - 5:22 AM
-- Time ended: 11/19/2012 - 5:31 AM
-------------------------------

-- Change the N to the current problem
module Euler4 where

import Data.Char

isPalindrome :: Integer -> Bool
isPalindrome num = do
	let list = map digitToInt $ show num
	reverse list == list

e4 = maximum [x * y | x <- [100..999], y <- [100..999], isPalindrome (x * y)]

-- Call the main function to get the result
main = e4