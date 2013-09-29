module Wk4Assignments

where

import System.Random
import Data.List
import SetOrd
import Week4

-- Assignment 2 ---------------------------------------------				
randomIntSetGen :: Int -> Set Int
randomIntSetGen l = list2set (take l $ randoms (mkStdGen 10) :: [Int])

setCount :: Set a -> Int
setCount (Set []) = 0
setCount (Set (x:xs)) = 1 + setCount(Set xs)

testSetCount :: Bool
testSetCount = setCount xs == 3
			   where xs = Set [2,5,7]

testRandomIntSetGen :: Int -> Bool
testRandomIntSetGen l = (setCount (randomIntSetGen l)) == l
-------------------------------------------------------------

-- Assignment 3 -------------------------------------------------------------------------

-- automatic random test ------------------------------
-- it can be used as: autoRandomTest numberOfTests randomSetTest numberOfGeneratedSets
-- it generates a list of all test results
-- Example: autoRandomTestUnionSet 5 randomUnionSetTest 7
autoRandomTest :: Int -> (Int -> Bool) -> Int -> [Bool] 
autoRandomTest n f m | n <= 0 = []
						   | otherwise = let n' = n - 1 
                             in ((f m) : (autoRandomTest n' f m))

-- UnionSet Start ----------------------------------------------
{-  unionSet is already implemented in SetOrd.hs!! ;) 
	unionSet :: (Ord a) => Set a -> Set a -> Set a 
	unionSet (Set [])     set2  =  set2
	unionSet (Set (x:xs)) set2  = insertSet x (unionSet (Set xs) set2)
-}

randomUnionSetTest :: Int -> Bool							 
randomUnionSetTest n = let 
               set1 = [x | x <- set2list(randomIntSetGen n)]
               set2 = [x | x <- set2list(randomIntSetGen n)]			  
			   in  (testUnionSet (list2set(set1)) (list2set(set2)) (list2set (nub (set1 ++ set2))))

testUnionSet :: (Ord a) => Set a -> Set a -> Set a -> Bool
testUnionSet set1 set2 unSet = (unionSet set1 set2) == unSet
-- UnionSet End ------------------------------------------------

-- intersectionSet Start ----------------------------------------------
-- intersectionSet
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) set2 = Set []
intersectSet (Set (x:xs)) set2      
						| inSet x sortedSet = insertSet x (intersectSet (Set xs) set2)
						| otherwise = intersectSet(Set xs) set2
						where sortedSet = sortSet(set2)
						
randomIntersectSetTest :: Int -> Bool							 
randomIntersectSetTest n = let 
               set1 = sort [x | x <- set2list(randomIntSetGen n)]
               set2 = sort [x | x <- set2list(randomIntSetGen n)]			  
			   in  (testIntersectSet (list2set(set1)) (list2set(set2)) (list2set (nub (createIntersection set1 set2))))

testIntersectSet :: (Ord a) => Set a -> Set a -> Set a -> Bool
testIntersectSet set1 set2 insSet = (intersectSet set1 set2) == insSet

createIntersection :: Eq a => [a] -> [a] -> [a]			   
createIntersection [] s = []		   
createIntersection (x:xs) s | elem x s = x : createIntersection xs s
								 | otherwise = createIntersection xs s

-- intersectionSet End -------------------------------------------------

-- diffSet Start ----------------------------------------------
-- diffSet						
diffSet :: (Ord a) => Set a -> Set a -> Set a
diffSet (Set []) set2  =  emptySet
diffSet (Set (x:xs)) set2 | inSet x sortedSet = diffSet (Set xs) set2
						  | otherwise = insertSet x (diffSet (Set xs) set2)
						  where sortedSet = sortSet(set2)

randomDiffSetTest :: Int -> Bool							 
randomDiffSetTest n = let 
               set1 = sort [x | x <- set2list(randomIntSetGen n)]
               set2 = sort [x | x <- set2list(randomIntSetGen n)]			  
			   in  (testDiffSet (list2set(set1)) (list2set(set2)) (list2set (nub (createDifferenceList set1 set2))))

testDiffSet :: (Ord a) => Set a -> Set a -> Set a -> Bool
testDiffSet set1 set2 difSet = (diffSet set1 set2) == difSet

createDifferenceList :: Eq a => [a] -> [a] -> [a]			   
createDifferenceList [] s = []		   
createDifferenceList (x:xs) s | elem x s = createDifferenceList xs s
								 | otherwise = x : createDifferenceList xs s
							 
-- diffSet End ----------------------------------------------		 
-- aux functions--------------------------------
sortSet :: (Ord a) => Set a -> Set a
sortSet (Set []) = emptySet
sortSet (Set (x:xs)) = list2set (sort (x : xs))

set2list :: (Ord a) => Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = (x:xs)

-------------------------------------------------
           

--------------------------------------------------------------

					  