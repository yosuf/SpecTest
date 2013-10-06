module Assignment1

where

import Data.List
import Week5

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 (\ unsorted sorted -> (length unsorted) == (length sorted)) mergeSrt

{-
    TimeSpend = 30 minutes
-}