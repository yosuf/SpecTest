module Assignment3 where

import System.IO.Unsafe  -- be careful!
import Data.List
import System.Random
import Data.Char
import Techniques


{- Assignment 3 Generator for random integer lists-}

{- Prajan -}
genIntList :: IO [Int]
genIntList =  do d <- getRandomInt 5
                 genIntListFs d
                 

genIntListFs :: Int -> IO [Int]
genIntListFs 0 = return []
genIntListFs n = do f  <- getRandomInt n 
                    fs <- genIntListFs (n -1)
                    return (f:fs)


{-Demi-}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

genIntList' :: Int -> Int -> IO [Int]
genIntList' _ 0 = return []
genIntList' d n = do
                f <- getRandomInt d
                fs <- genIntList' d (n-1)
                return (f:fs)

genIntList'' :: IO [Int]
genIntList'' = genIntList' 100 20

{-Jeroen-}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

genIntList :: IO [Int]
genIntList =  getStdRandom rndListGen

rndListGen :: StdGen -> ([Int], StdGen)
rndListGen gen = (rndList gen, snd(next gen))
rndList gen = let t = next gen
              in (fst (t)):(rndList (snd t))





