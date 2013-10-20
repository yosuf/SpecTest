module Assignment1

where

import Data.List
import Week5Adapted
-- VVZ: had to fix the name of the imported module, looks like you didn't run the code before deployment

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtA :: Ord a => [a] -> [a]
mergeSrtA = assert1 lengthProp mergeSrt
-- VVZ: a pretty weak property. could have used 'mergeSrtA2 = post1 sorted mergeSrt' instead
-- VVZ: (or 'assert1 (\ _ ys -> sorted ys) mergeSrt', whatever you prefer)

lengthProp xs ys = (length xs) == (length ys)

{-
    TimeSpend = 30 minutes
-}