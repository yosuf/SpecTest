module LabExam

where
import Data.List
import Assert

{- 	
	Author: Yosuf Haydary (yosuf.haydary@gmail.com)
	Student number: 10411119
-}


{-
===================
	Question 1
===================
-}
f :: (Integer,Integer) -> (Integer,Integer)
f = until (odd.snd) (\ (m,n) -> (m+1,n `div` 2))

-- part 1
--Precondition for f: snd>0
fA = assert1 (\ (s,r) (t,u) ->  u*(2^(t-s)) == r && r>=u && s<=t && odd u ) f

{-
- part 2
Explanation: 
Each time the snd part of the pair is divided by two (if not odd) the fst part of the pair is incremented by one.
The difference of the fst of the pair shows how many times the snd of the pair is divided by two. 
So, this difference is the power to 2 to which the snd of the pair can be multiplied to get the original back.

At the same time, the snd should be odd, fst should be greater or equal to original and snd should be smaller or equal.
-}


{-
===================
	Question 2
===================
-}

data BinTree a = Nil | B a (BinTree a) (BinTree a) deriving (Eq,Show)
data Btree a = Leaf a | Node (Btree a) (Btree a) deriving (Eq,Show)

--some tree examples
binTree1 = B 1 (Nil) (Nil)
binTree2 = B 2 binTree1 binTree1
bTree1 = Node (Leaf 1) (Leaf 1)
bTree2 = Node bTree1 bTree1

--part 1
bintree2btree :: Eq a => a -> BinTree a -> Btree a
bintree2btree x Nil 				= Leaf x
bintree2btree x (B _ Nil Nil) 		= Node (Leaf x) (Leaf x)
bintree2btree x (B _ binT1 binT2) 	= Node (bintree2btree x binT1) (bintree2btree x binT2)


--part 2
btree2bintree :: Eq a => a -> Btree a -> BinTree a
btree2bintree x (Leaf _) 					= Nil
btree2bintree x (Node (Leaf _) (Leaf _))	= B x Nil Nil
btree2bintree x (Node (bT1)      (bT2))		= B x (btree2bintree x bT1) (btree2bintree x bT2)

{- part 3
Next, show how you can test these two functions for correctness.

One of the ways to check these funtions is to compare the tree structure of the produced tree with the original tree, 
which should be the same.
If a BinTree is converted to a Btree, the leaves of the Btree should all contain only the given arg.
If a BTree is converted to a BinTree, the internal nodes of the BinTree should only contain the given arg.


Another way to test a bintree2Btree is by making an assertion wrappen.
This should re-convert the converted BTree to BinTree and it should be the same as the original.
But it will only work if The internal nodes, leaves and the given args exactly are equivalent.
bintree2btreeA :: Eq a => a -> BinTree a -> Btree a
bintree2btreeA x bintree = assert2  (\x1 bintree1 btree1 -> (btree2bintree x1 btree1) == bintree1 ) bintree2btree x bintree
-}





{-
===================
	Question 3
===================

--part 1
In-order traversal of a binary tree visits the nodes of the tree by first doing an in-order traversal of the left subtree, 
then visiting the root node, and next doing an in-order traversal of the right subtree. 

Implement a function 
inOrder :: BinTree a -> [a] 
that collects the items found by in-order traversal of a binary tree in a list.
-}

inOrder :: BinTree a -> [a]
inOrder Nil = []
inOrder (B x Nil Nil) 		= [x]
inOrder (B x binT1 binT2)	= inOrder binT1 ++ [x] ++ inOrder binT2


{-part 2
Next define a second function for in-order traversal in right to left direction:
-}
inOrderRev :: BinTree a -> [a]
inOrderRev (B x Nil Nil) 		= [x]
inOrderRev (B x binT1 binT2)	= inOrder binT2 ++ [x] ++ inOrder binT1

{- 
part 3
Finally, fill in the dots to define a property that can be used to test the two functions by relating them to each other.
-}
treeProperty :: Eq a => BinTree a -> Bool
treeProperty t = inOrder t == (reverse $ inOrderRev t)


{-
===================
	Question 4
===================

A dictionary is a binary tree with pairs of type (String,String) at its nodes.
The idea is that the second string gives a translation or an explanation of the first string.

A dictionary is ordered if all items on the left subtree have keys that are alphabetically before the key at the root node, 
and all items on the right subtree have keys that are alphabetically after the key at the root node, 
and moreover the left and right subtrees are also ordered.

Write a test property ordered :: Dict -> Bool for this. (Hint: consider using the inOrder function from the previous exercise.)

-}

ordered :: Dict -> Bool
ordered dict = ordered' (inOrder dict) 

ordered' [] = True
ordered' [x] = True
ordered' (x:y:xs) = (fst x) < (fst y) && ordered' (y:xs)



{-
===================
	Question 5
===================
-}

type Dict = BinTree (String,String) 

key, value :: (String,String) -> String
key (x,_) = x
value  (_,y) = y 

--dictionary examples
unorderedDict = B ("A","Letter A") (B ("B", "Letter B") Nil Nil) Nil
orderedDict = B ("D","Letter D") (B ("C", "Letter C") Nil Nil) Nil

{-
Implement a function lookUp :: String -> Dict -> [String] that looks up a key in an ordered dictionary. 
Make sure the lookup function exploits the order. 
An output [] indicates that the key is not defined in the dictionary, 
a non-empty list gives the value for a given key. 
Recall that the items in the dictionary tree have the form (key,value). 

You can assume each key occurs at most once in the dictionary.
-}

--precondition: Dictionary is ordered
lookUp :: String -> Dict -> [String]
lookUp looking Nil = []
lookUp looking (B dictEntry b1 b2 )  = if looking == (key dictEntry) then [(value dictEntry)]
											else if looking < (key dictEntry) then lookUp looking b1
												else lookUp looking b2


{-
===================
	Question 6
===================

- part 1
Write code for inserting a new item at the correct position in an ordered dictionary. 
If the key of the item already occurs in the dictionary, replace the old information with the new information, 
otherwise just insert the new item. 
The type should be insertKey :: (String,String) -> Dict -> Dict

Usage Example: inOrder (insertKey ("Z","Letter Z") orderedDict )
-}

insertKey :: (String,String) -> Dict -> Dict
insertKey entry Nil = B entry Nil Nil
insertKey entry (B dictEntry b1 b2) = if (key entry) == (key dictEntry) then B entry b1 b2
										else if (key entry) < (key dictEntry) then B dictEntry (insertKey entry b1) b2
											else B dictEntry b1 (insertKey entry b2)


{- part 2
Next, write an assertive version of this that checks whether an ordered dictionary is still ordered after the insertion. 
Use the property ordered :: Dict -> Bool that you defined earlier.
-}

insertKeyA :: (String,String) -> Dict -> Dict
insertKeyA entry dict = assert0 (\d -> ordered d) insertKey entry dict

assert0 :: (a -> Bool) -> (b -> a -> a) -> b -> a -> a
assert0 p f x y = if p (f x y) then f x y
                else error "assert0"




