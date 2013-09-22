module Assignment3 where

import System.IO.Unsafe  -- be careful!
import System.Random
import Data.List


getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

genIntList :: IO [Int]
genIntList =  getStdRandom rndListGen

rndListGen :: StdGen -> ([Int], StdGen)
rndListGen gen = (rndList gen, snd(next gen))
rndList gen = let t = next gen
              in (fst (t)):(rndList (snd t))
              
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []     = True
isPermutation ys xs     | length ys /= length xs        = False
                        | (length.removeAll ys) xs == 0 = True
                        | otherwise = False

removeAll [] xs = xs
removeAll (y:ys) xs = removeAll ys (removeFst y xs)
                        
removeFst _ [] = []
removeFst y (x:xs)  | y == x = xs
                    | otherwise = x:(removeFst y xs)
                    

--shuffle xs =  let x = xs !! (unsafePerformIO (getRandomInt ((length xs) -1)))+1
--              in x : (removeFst x xs)

testPermRev :: [Int] -> [Int]
testPermRev xs = if isPermutation xs (reverse (xs)) /= True then error "not Perm" else xs
  
    
testIsPermutation = 
  do
    putStrLn "Testing IsPermutation..."
    
    x <- genIntList
    let x1 = take 10 x
    let t1 = testPermRev x1
    --isPermutation rndList' rndList'
    putStrLn "Ready!"