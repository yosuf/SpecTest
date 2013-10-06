module Week4 where

import SetOrd
import System.Random
import Techniques
import Data.List

-- Exercise 3

intersectionSet :: (Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) _ = Set []
intersectionSet (Set (x:xs)) (Set ys) | (inSet x (Set (sort ys))) = insertSet x (intersectionSet (Set xs) (Set ys))
				      | otherwise = intersectionSet (Set xs) (Set ys)



differenceSet' :: (Ord a) => Set a -> Set a -> Set a
differenceSet' (Set []) _ = Set []
differenceSet' (Set (x:xs)) set2 | (not (inSet x set2)) = insertSet x (differenceSet' (Set xs) set2)
                                 | otherwise = differenceSet' (Set xs) set2
  
differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = unionSet (differenceSet' (Set xs) (Set ys)) (differenceSet' (Set ys) (Set xs))

-- Exercise 4


type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Eq a => Rel a -> Rel a
trClos [] = []
trClos (x:xs) = ([x] ++ ([x] @@ (trClos xs))) ++ (trClos xs)  




