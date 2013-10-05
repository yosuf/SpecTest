module Assignment3

where

import System.Random
import Data.List
import SetOrd
import Week4
import Assignment2
import Utils

-- automatic random test Start --------------------------------------------------------------
-- it can be used as: autoRandomTest numberOfTests randomSetTest numberOfGeneratedSets
-- it generates a list of all test results
-- Example: autoRandomTestUnionSet 5 randomUnionSetTest 7
autoRandomTest :: Int -> (Int -> Bool) -> Int -> [Bool] 
autoRandomTest n f m | n <= 0 = []
						   | otherwise = let n' = n - 1 
                             in ((f m) : (autoRandomTest n' f m))
-- automatic random test End ----------------------------------------------------------------


-- UnionSet Start ---------------------------------------------------------------------------
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
-- UnionSet End ------------------------------------------------------------------------------


-- intersectionSet Start ---------------------------------------------------------------------
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

-- intersectionSet End -------------------------------------------------------------------------


-- diffSet Start -------------------------------------------------------------------------------
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
							 
-- diffSet End -----------------------------------------------------------------------------------

		 
-- aux functions--------------------------------
sortSet :: (Ord a) => Set a -> Set a
sortSet (Set []) = emptySet
sortSet (Set (x:xs)) = list2set (sort (x : xs))

set2list :: (Ord a) => Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = (x:xs)

-------------------------------------------------


-- Intersection: 1,5 hours
intersection :: Eq a => Set a -> Set a -> Set a
intersection (Set []) set2			= emptySet
intersection set1 (Set [])			= emptySet
intersection (Set (xx)) set2		= Set [x | x <- xx , isEelem x set2]
-- All the results should be true
testIntersection :: [Bool]
testIntersection = [ intersection (list2set [1,2,3,4]) (list2set [3,4,5,6]) == (list2set [3,4]),
					 intersection (list2set [1,2,3,4]) (list2set [5,6,7]) == (emptySet)
					 ]

-- Union: 1 hour
union' ::  Eq a => Ord a => Set a -> Set a -> Set a
union' (Set x) (Set y) = list2set (x++y)
-- simple tests ;)
testUnion :: [Bool]
testUnion =	[ not $ isEmpty (union' (Set [1]) (Set [2])), 
			  isEmpty $ union' (Set emptyList) (Set emptyList) 
			]

-- Difference: 15 min
-- returns all the elements of set1 not present in set2
difference :: Eq a => Set a -> Set a -> Set a
difference (Set []) set2 = emptySet
difference set1 (Set []) = set1
difference (Set (xx)) set2		= Set [x | x <- xx , not $ isEelem x set2]
-- trivial tests
testDifference :: [Bool]
testDifference = [	difference (list2set [1,2,3,4]) (list2set [3,4,5,6]) == (list2set [1,2]),
					difference (list2set [1,2,3,4]) (list2set [5,6,7]) == (list2set [1,2,3,4]),
					isEmpty $ difference (Set emptyList) (Set emptyList)
				]


-- So this should always be true since  intersection of any set with itself is itself. 
randomTestIntersection :: IO ()
randomTestIntersection = do 
		randomList <- genIntList
		putStrLn $ show $ assertTrue ( intersection (list2set randomList) (list2set randomList) == (list2set randomList))

randomTestIntersection2 :: IO ()
randomTestIntersection2 = do 
		list1 <- genIntList
		list2 <- genIntList
		putStrLn $ show $ assertTrue (subSet (intersection (list2set list1) (list2set list2) ) (list2set list2) && subSet (intersection (list2set list1) (list2set list2) ) (list2set list1))


-- Checks whether an a is an element of the given set.
isEelem :: Eq a => a -> Set a -> Bool
isEelem x (Set []) = False
isEelem x (Set xx) = elem x xx
-- All should be true
testIsElem :: [Bool]
testIsElem = [ isEelem 1 (Set [1]), isEelem 1 (Set [1,1]), not $ isEelem 1 (Set []), not $ isEelem 1 (Set [2,2])  ]


-- just a workaround ;) because [] is not recognized as Ord,Eq instance
emptyList :: [Integer]
emptyList = []

-- returns the count/size of the set
countSet :: Set a -> Integer
countSet (Set [])		= 0
countSet (Set (x:xx))	= 1 + (countSet $ Set xx)
-- we could use some random sets to check that it is never smaller than 0 ;)
testCountSet :: [Bool]
testCountSet = [ 0 == countSet emptySet , 2 == countSet (list2set [1,2]) ]


           