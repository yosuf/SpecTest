module Assignment3 where

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
removeAll (y:ys) xs = removeAll ys (delete y xs)

testIsPermutation :: ([Bool] -> Bool) -> ([[Int] -> [Int]]) -> (IO Bool)
testIsPermutation _ [] = fail "Provide transformational list functions"
testIsPermutation t fs = do l <- genIntList
                            let lcap = take 100 l
                            let y = t [ isPermutation lcap (f lcap) | f <- fs ]
                            return y

duplicate fs = fs ++ fs
                            
testAll = do
            t1 <- testIsPermutation and [reverse, sort]
            t2 <-  testIsPermutation (not.or) [duplicate]
            let t = t1 && t2
            return t