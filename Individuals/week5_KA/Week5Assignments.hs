module Lab5

where

import Data.List
import Week5

-- assign 1
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtAssert1 :: ([a] -> Bool) -> ([a] -> [a]) -> [a] -> [a]
mergeSrtAssert1 postCond f xs = if postCond (f xs) then f xs
						  else error "assertSorted"
						  
assertiveMergeSrt :: Ord a => [a] -> [a]
assertiveMergeSrt xs = mergeSrtAssert1 sorted mergeSrt xs

