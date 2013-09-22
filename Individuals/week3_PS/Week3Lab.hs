module Week3Lab where
import Data.List
import Data.Char
import System.Random
import Techniques




{- 3 Generator for random integer lists -}

genIntList :: IO [Int]
genIntList =  do d <- getRandomInt 5
                 genIntListFs d
                 

genIntListFs :: Int -> IO [Int]
genIntListFs 0 = return []
genIntListFs n = do f  <- getRandomInt n 
                    fs <- genIntListFs (n -1)
                    return (f:fs)

{- 4 Compare Permutation and generate True if arguments are permutations of each other -}

isPermutation :: Eq a => [a] => [a] -> Bool   {- isPermutation [2,1,3] [1,2,3] : Reuslt is True-}
isPermutation [] [] = True
isPermutation (x:xs) ys | (length (x:xs) /= length ys) = error "List parameters not equal "
                        |  x `elem` ys = isPermutation xs (delArgInList (==x) ys)
                        |  otherwise = False


delArgInList ::  (a -> Bool) -> [a] -> [a]   {- this is delete argument in list compare (== 1) [1,2,3,4]    : Result is [1,3,4]-}
delArgInList p [] = []
delArgInList p (x:xs) | p x      = xs
                    | otherwise = x : delArgInList p xs

