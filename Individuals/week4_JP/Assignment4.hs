module Assignment4

where

import Data.List (nub)
import SetOrd
import Assignment3

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

s2l (Set xs) = xs
l2s xs = list2set xs

trClos :: Ord a => Rel a -> Rel a
trClos []   = []
trClos (xs) | isEmpty (diff (l2s xs) (l2s(xs @@ xs)))  = xs
            | otherwise                                = trClos (s2l (union (l2s xs) (l2s (xs @@ xs))))

rels =  [(1,2),(2,3),(3,4)]