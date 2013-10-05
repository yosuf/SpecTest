module Utils where

import System.Random

{- A lightweigh multifunctional utils class 
@Author: YH-}

-- While testing, this assert could be used to check for failures in a readable mode
assertTrue :: Bool -> String
assertTrue False = error "Assertion to be True failed."
assertTrue _	 = "Assertion to be True succeeded."

assertFalse :: Bool -> String
assertFalse True = error "Assertion to be False failed."
assertFalse _	 = "Assertion to be False succeeded."


-- returns the number of 'a' present in the given list
-- Example count 1 [1,1,0,0] will return 2
count :: Eq a => a -> [a] -> Integer
count a [] = 0
count a (x:rest) = if a == x then 1+(count a rest) else (count a rest)

-- This method should return a list containing only True
-- Otherwise the count method is faulty
testCount :: [Bool]
testCount = [ 	0 == count 1 [], 
				1 == count 2 [9,2,3],
				4 == count 0 [9,0,3,0,5,0,1,0] ]



removeDuplicates:: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xx) = if elem x xx then removeDuplicates xx else x:(removeDuplicates xx)


-- "Random" List generation by Demi
genIntList' :: Int -> Int -> IO [Int]
genIntList' _ 0 = return []
genIntList' d n = do
                f <- getRandomInt d
                fs <- genIntList' d (n-1)
                return (f:fs)

genIntList :: IO [Int]
genIntList = genIntList' 100 20

-- from Techniques of Jan.
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))