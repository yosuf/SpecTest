module Assignment2

where

import System.Random
import Data.List
import SetOrd
import Week4

-- Assignment 1 ---------------------------------------------				
randomIntSetGen :: Int -> Set Int
randomIntSetGen l = list2set (take l $ randoms (mkStdGen 1000) :: [Int])

setCount :: Set a -> Int
setCount (Set []) = 0
setCount (Set (x:xs)) = 1 + setCount(Set xs)

testSetCount :: Bool
testSetCount = setCount xs == 3
			   where xs = Set [2,5,7]

testRandomIntSetGen :: Int -> Bool
testRandomIntSetGen l = (setCount (randomIntSetGen l)) == l
-------------------------------------------------------------