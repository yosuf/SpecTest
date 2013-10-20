module Assignment2

where

import Data.List
import Week5Adapted
-- VVZ: had to fix the name of the imported module, looks like you didn't run the code before deployment

split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2 in (take n xs, drop n xs)

splitSort :: Ord a => [a] -> [a]
splitSort xs =  let s = split xs
                in merge (sort (fst s)) (sort (snd s))
-- VVZ: sort, not splitSort? seriously?
-- VVZ: you're "sorting" by actually using another function to sort?
-- VVZ: the reason your recursive version doesn't terminate, is the lack of the base condition (e.g., empty list and/or trivial list)

elemOf e = length.filter (== e)

lengthProp xs ys = (length xs) == (length ys)
elemProp xs ys = and [elemOf e xs == elemOf e ys | e <- xs]

splitSortA :: Ord a => [a] -> [a]
splitSortA = assert1 elemProp 
                $ assert1 lengthProp splitSort

{-
    TimeSpend = 45 minutes
-}