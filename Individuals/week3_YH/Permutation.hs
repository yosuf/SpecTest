module Permutation where

import Data.List
import Utils



{- 
Assignment 4
Time taken: 3 hours

Assignment: A permutation of a finite list is another finite list with the same elements, but possibly in a different order.
For example, [0,2,0] is a
permutation of [0,0,2], but [2,2,0] is not. Write a function
isPermutation :: Eq a => [a] -> [a] -> Bool
that returns True if its arguments are permutations of each other.
-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs xy	| (length xs) /= (length xy) = False
					| otherwise = and [ count s xs == count s xy | s <- xs ]


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


