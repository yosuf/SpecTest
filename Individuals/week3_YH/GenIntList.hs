
module GenIntList where
	

import System.Random

{-
Assignment 3
Time taken: 
Consult the course slides of this week to write a generator for random
integer lists. The type should be genIntList :: IO [Int]
-}

genIntList :: IO [Int]
genIntList = do
			rndInt <- genRndInt
			(genIntList' rndInt [])


genIntList' :: Int -> [Int] -> IO [Int]
genIntList' 0 l = return l
genIntList' n l = do 
			rndInt <- genRndInt
			genIntList' (n-1) (rndInt:l)


genRndInt :: IO Int
genRndInt = getStdRandom (randomR (0,  20::Int ))




--testGetIntList :: [Bool]
--testGetIntList = [genIntList /= genIntList | x <- [1..100] ]

