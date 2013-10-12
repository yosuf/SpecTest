module Workshop6YH where

import Data.List
import System.Random
--import IAR
import Week6

data Btree a = Leaf a | Node (Btree a) (Btree a) deriving (Eq,Show)
data Tree a = T a [Tree a] deriving (Eq,Ord,Show)


leafCount :: Btree a -> Integer
leafCount (Leaf _) = 1
leafCount (Node t1 t2) = leafCount t1 + leafCount t2



mapB :: (a -> b) -> Btree a -> Btree b
mapB f (Leaf leaf) = Leaf (f leaf)
mapB f (Node t1 t2) = Node (mapB f t1) (mapB f t2)


depthB :: Btree a -> Integer
depthB (Leaf _) = 0
depthB (Node t1 t2) = max (depthB t1) (depthB t2) + 1


balancedB :: Btree a -> Bool
balancedB (Leaf _) = True
balancedB (Node t1 t2) = balancedB t1 && balancedB t2 && depthB t1 == depthB t2



--count :: Tree a -> Int
--count (T _ xs) = length xs 

depth :: Tree a -> Int
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1


mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T s ts) = T (f s) (map (mapT f) ts)



--Trees
balanced = ( Node (Leaf 1) (Leaf 2) )
unbalanced = ( Node (Leaf 2) (Node (Leaf 3) (Leaf 3))  )

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]
example3 = T 3 [example1,example2,example2]
