module Assignment4_5 where

import Data.List
import Utils

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
(Deliverable: Haskell program, indication of time spent.) -}

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r , (w,z) <- s , y == w]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos(x:xs) = nub $ ([x] ++ ([x] @@ (trClos xs))) ++ (trClos xs)


{- Assignment 5
Test the function trClos from the previous exercise. Devise your own
test method for this. Try to use random test generation. Define reasonable properties to test.
(Deliverables: test code, short test report, indication of time spent.)
-}

--Example given in the Assignment
testTrClose':: IO ()
testTrClose' = putStrLn $ show $ assertTrue (trClos [(1,2),(2,3),(3,4)] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)] )

testTrClose'' :: IO ()
testTrClose'' = do -- These two should exactly be the same since the x sets are included always in y
	putStrLn $ show $ assertTrue (trClos [(x,y) | x <- [1..5] , y <- [1..4] ] == [(x,y) | x <- [1..5] , y<-[1..4]] )

testTrClose :: Int -> Int -> IO ()
testTrClose a b = if a < 0 || b < 0 then error "Only positive numbers allowed" 
	else do
	putStrLn $ show $ assertTrue ( (length $ trClos [(x,y) | x <- [1..a] , y <- [1..b] ]) == a*b )


