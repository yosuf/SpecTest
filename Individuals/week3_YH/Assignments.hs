module Permutation where

import Data.List
import System.IO
import System.Random
import Control.Applicative


{- 
Yosuf Haydary
Assignment 4
Time taken: 3 hours
-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs xy	| (length xs) /= (length xy) = False
					| otherwise = and [and [elem s xy, elem y xs] && count s xs == count s xy| s <- xs, y <- xy ]


testIsPermutation :: [ Bool ]
testIsPermutation = [ 	isPermutation (delete 1 [1]) (delete 1 [1]), 
						isPermutation [1] [1] ,
						isPermutation [2,2] [2,2], 
						isPermutation [0,2,0] [0,0,2], -- Example from the lab assignment
						isPermutation [3,3,3] [3,3,3],
						isPermutation [5,5,5,5,5] [5,5,5,5,5], 
						isPermutation [0..10] [10, 9,8,7,6,5,4,3,2,1,0] ]
						
testIsNotPermutation :: [Bool]
testIsNotPermutation =  [ 	isPermutation [1] [1,2],
							isPermutation (delete 1 [1]) [1], 
							isPermutation [0,2,0] [2,2,0], -- Example from the lab
							isPermutation [1,2,2,3,3,3,4,4,4,4] [4,4,4,4,3,3,3,2,1,1],
							isPermutation [0..10] [1..11] ]


count :: Eq a => a -> [a] -> Integer
count a [] = 0
count a (x:rest) = if a == x then 1+(count a rest) else (count a rest)
