module Assignment4_5 where

import Data.List

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
trClos(x:xs) = ([x] ++ ([x] @@ (trClos xs))) ++ (trClos xs)


{- Assignment 5
Test the function trClos from the previous exercise. Devise your own
test method for this. Try to use random test generation. Dene reasonable properties to test.
(Deliverables: test code, short test report, indication of time spent.)
-}



