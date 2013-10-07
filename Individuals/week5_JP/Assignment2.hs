module Assignment2

where

import Data.List
import Week5Adapted

split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2 in (take n xs, drop n xs)

splitSort :: Ord a => [a] -> [a]
splitSort xs =  let s = split xs
                in merge (sort (fst s)) (sort (snd s))

elemOf e = length.filter (== e)

lengthProp xs ys = (length xs) == (length ys)
elemProp xs ys = and [elemOf e xs == elemOf e ys | e <- xs]

splitSortA :: Ord a => [a] -> [a]
splitSortA = assert1 elemProp 
                $ assert1 lengthProp splitSort

{-
    TimeSpend = 45 minutes
-}