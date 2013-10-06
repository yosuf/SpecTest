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

-- assign 2
split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2
		   in (take n xs, drop n xs)

splitMergeSrt :: Ord a => [a] -> [a]
splitMergeSrt [] = []
splitMergeSrt [x] = [x]
splitMergeSrt xs = 
				let                    
					fstP = (fst (split xs))
					sndP = (snd (split xs))					
				in (merge (splitMergeSrt fstP) (splitMergeSrt sndP))
 
assertiveSplitMergeSrt :: Ord a => [a] -> [a]
assertiveSplitMergeSrt xs = mergeSrtAssert1 sorted splitMergeSrt xs