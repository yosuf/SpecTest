module Assignment4_5 where

import Data.List
import Assignment3

{- Please look below for assignment 5-}

{-  Assignment: A permutation of a finite list is another finite list with the same elements, but possibly in a different order.
For example, [0,2,0] is a
permutation of [0,0,2], but [2,2,0] is not. Write a function
isPermutation :: Eq a => [a] -> [a] -> Bool
that returns True if its arguments are permutations of each other.-}

{- Note: There are multiple variations of this assignment. All of them are noted below -}



{-  Yosuf Haydary. Time taken: 3 hours -}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs xy	| (length xs) /= (length xy) = False
					| otherwise = and [ count s xs == count s xy | s <- xs ]


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

{- Assignment 5:
Define some testable properties for this function, and use your random
generator for integer lists from Exercise 3 to test isPermutation.-}

{-- Time taken: 6 hours because of IO--}

-- The returned list should only contain True.
testIsPermutation ::[ Bool ]
testIsPermutation = [	isPermutation (delete 1 [1]) (delete 1 [1]), 
						isPermutation [1] [1] ,
						isPermutation [2,2] [2,2], 
						isPermutation [0,2,0] [0,0,2], -- Example from the lab assignment
						isPermutation [3,3,3] [3,3,3],
						isPermutation [5,5,5,5,5] [5,5,5,5,5], 
						isPermutation [0..10] [10, 9,8,7,6,5,4,3,2,1,0] 
					]
						
-- The returned list should only contain False.
testIsNotPermutation :: [ Bool ]
testIsNotPermutation =  [ 	isPermutation [] [0], 
							isPermutation [1] [1,2],
							isPermutation (delete 1 [1]) [1], 
							isPermutation [0,2,0] [2,2,0], -- Example from the lab
							isPermutation [1,2,2,3,3,3,4,4,4,4] [4,4,4,4,3,3,3,2,1,1],
							isPermutation [0..10] [1..11] 
						]


testRandomPermutation :: IO ()
testRandomPermutation = do
		list1 <- genIntList''
		list2 <- genIntList''
		putStrLn $ "list1 list2 " ++ show (isPermutation list1 list2) ++ " reverse list1: " ++ show (isPermutation list1 (reverse list1)) ++ " reverse list2: " ++ show (isPermutation list2 (reverse list2))


-- Jeroen
testIsPermutation' :: ([Bool] -> Bool) -> ([[Int] -> [Int]]) -> (IO Bool)
testIsPermutation' _ [] = fail "Provide transformational list functions"
testIsPermutation' t fs = do  l <- genIntList
                              let lcap = take 100 l
                              let y = t [ isPermutation lcap (f lcap) | f <- fs ]
                              return y
duplicate fs = fs ++ fs
                            
testAll = do
            t1 <- testIsPermutation' and [reverse, sort]
            t2 <-  testIsPermutation' (not.or) [duplicate]
            let t = t1 && t2
            return t

