module Assignment1

where

import Data.List
import Week5

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 lengthProp mergeSrt

lengthProp xs ys = (length xs) == (length ys)

{-
    TimeSpend = 30 minutes
-}