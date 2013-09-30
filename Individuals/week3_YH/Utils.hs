module Utils where

-- returns the number of a present in the given list
-- Example count 1 [1,1,0,0] will return 2
count :: Eq a => a -> [a] -> Integer
count a [] = 0
count a (x:rest) = if a == x then 1+(count a rest) else (count a rest)

-- This method should return a list containing only True
-- Otherwise the count method is faulty
testCount :: [Bool]
testCount = [ 	0 == count 1 [], 
				1 == count 2 [9,2,3],
				4 == count 0 [9,0,3,0,5,0,1,0] ]



removeDuplicates:: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xx) = if elem x xx then removeDuplicates xx else x:(removeDuplicates xx)