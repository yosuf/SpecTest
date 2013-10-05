module Assignments where

import SetOrd
import Utils
--Assignment3 comes from last week. Needed here to generate random lists.

{-- A2 --}
{-- Description --}
{- 
- Implement a random data generator for the datatype Set Int, 
- where Set is as defined in http://homepages.cwi.nl/~jve/rcrh/SetOrd.hs.
- (Deliverables: Haskell program, indication of time spent.) -}

generateRandomSet :: IO ()
generateRandomSet = do 
		randomList <- genIntList
		putStrLn $ show $ list2set randomList


{- A3-}
{- Deliverables: 
- Haskell program
- test code
- short test report
- indication of time spent.

- Implement operations for 
	* set intersection,
	* set union and 
	* set difference, 
	* for the datatype Set defined in http://homepages.cwi.nl/~jve/rcrh/SetOrd.hs. 

Next
- use automated random testing to check that your implementation is correct. -}

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
union ::  Eq a => Ord a => Set a -> Set a -> Set a
union (Set x) (Set y) = list2set (x++y)
-- simple tests ;)
testUnion :: [Bool]
testUnion =	[ not $ isEmpty (union (Set [1]) (Set [2])), 
			  isEmpty $ union (Set emptyList) (Set emptyList) 
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


-- Checks whether an a is an element of the given set.
isEelem :: Eq a => a -> Set a -> Bool
isEelem x (Set []) = False
isEelem x (Set xx) = elem x xx
-- All should be true
testIsElem :: [Bool]
testIsElem = [ isEelem 1 (Set [1]), isEelem 1 (Set [1,1]), not $ isEelem 1 (Set []), not $ isEelem 1 (Set [2,2])  ]


-- returns the count/size of the set
countSet :: Set a -> Integer
countSet (Set [])		= 0
countSet (Set (x:xx))	= 1 + (countSet $ Set xx)
-- we could use some random sets to check that it is never smaller than 0 ;)
testCountSet :: [Bool]
testCountSet = [ 0 == countSet emptySet , 2 == countSet (list2set [1,2]) ]


-- just a workaround ;) because [] is not recognized as Ord,Eq instance
emptyList :: [Integer]
emptyList = []

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



{-Assignment 4
Suppose we implement binary relations as list of pairs, Haskell type [(a,a)].

Assume the following definitions:
type Rel a = [(a,a)]
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -`> Rel a
r @@ s =
	nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

Use this to implement a function
trClos :: Ord a => Rel a -> Rel a

that gives the transitive closure of a relation, where the relation is
represented as a list of pairs.
E.g., trClos [(1,2),(2,3),(3,4)] should give
[(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
(Deliverable: Haskell program, indication of time spent.)
-}

type Rel a = [(a,a)]
--infixr 5 @@

--(@@) :: Eq a => Rel a -> Rel a -> Rel a
--r @@ s =
--	nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]


ones = 1 : twos
twos = 2 : ones




{- Assignment 5
Test the function trClos from the previous exercise. Devise your own
test method for this. Try to use random test generation. Dene reasonable properties to test.
(Deliverables: test code, short test report, indication of time spent.)
-}
















reverse' :: [a] -> [a]
reverse' []  = []
--reverse' [x] = [x]
reverse' (x:xx) = reverse' xx ++ [x]
 


f :: Int -> Int
f y = f' y 0 0

f' :: Int -> Int -> Int -> Int
f' y n x = if n < y then
	let
		x' = x + 2*n + 1
		n' = n + 1
		in f' y n' x'
	else x

















