module LibLecture.Week4SetOrd (Set(..),emptySet,isEmpty,inSet,subSet,insertSet,
                               deleteSet,powerSet,powerList,takeSet,(!!!),list2set,unionSet) 

where

import Data.List (sort) 

{-- Sets implemented as ordered lists without duplicates --} 

newtype Set a = Set [a] deriving (Eq,Ord)

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

emptySet  :: Set a       
emptySet = Set []

isEmpty  :: Set a -> Bool            
isEmpty (Set []) = True
isEmpty _        = False

inSet  :: (Ord a) => a -> Set a -> Bool  
inSet x (Set s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (Set []) _       = True  
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set  {- ps: subSet (Set [1,2,3]) (Set [1,2,3,4]) : Return True -}

insertSet :: (Ord a) => a -> Set a -> Set a 
insertSet x (Set s) = Set (insertList x s)                   {-insertSet 7 (Set [1,2,3]) : Result {1,2,3,7} -}

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of                {- take first the input value , and first element of the set -}
                                 GT -> y : insertList x ys'  {- if input x is greater than that compared first value then only keep y and do a rcursive-}
                                 EQ -> ys                    {- if input x is equal to that value of y then leave that x and only keep ys-}
                                 _  -> x : ys                {- if not of both above then obviously LT , so keep x before that set-}

deleteSet :: Ord a => a -> Set a -> Set a 
deleteSet x (Set s) = Set (deleteList x s)

deleteList x [] = []
deleteList x ys@(y:ys') = case compare x y of 
                                 GT -> y : deleteList x ys'
                                 EQ -> ys'
                                 _  -> ys

list2set :: Ord a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)
-- list2set xs = Set (foldr insertList [] xs)

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) = 
   Set (sort (map (\xs -> (list2set xs)) (powerList xs)))


powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs)                  {- if only this then it returns [[]]-}
                     ++ (map (x:) (powerList xs))   {- if only this then it returns [[1,2]] -}

{-
[1,2]
  []
    [2]
     [1]
       [1,2]

  [2] ++ map (1:) [2]
    [] ++ map (2:) []


Step 1 
  [[]] ++ map (2:) [[]]  => [[]],[2]]
Step 2  
  [[],[2]] ++ map (1:) [[],[2]]   => [[],[2],[1],[1,2]]

-}




takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs) 

infixl 9 !!!  {- Called fixity declaration -}

(!!!) :: Eq a => Set a -> Int -> a    
(Set xs) !!! n = xs !! n

{- s !! n means accessing the nth character , starts with zero. 
   [7,5,6,2] !! 0 results 7
   [7,5,6,2] !! 1 results 5
-}

unionSet :: (Ord a) => Set a -> Set a -> Set a 
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  = 
   insertSet x (unionSet (Set xs) set2)
