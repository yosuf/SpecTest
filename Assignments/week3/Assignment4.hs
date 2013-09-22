module Assignment4 where

import System.IO.Unsafe  -- be careful!
import Data.List
import System.Random
import Data.Char
import Techniques



{- Assignment 4 Compare Permutation and generate True if arguments are permutations of each other-}

{- Yosuf: Time taken: 1hr -}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs xy = if (length xs) /= (length xy) then  False 
	                  else and [and [elem s xy, elem y xs] | s <- xs, y <- xy ]


{-Demi-}
isPermutation'' :: Eq a => [a] -> [a] -> Bool
isPermutation'' xs ys | length xs /= length ys = False
                    | xs == ys = True
                    | otherwise = or (map (==xs) (permutations ys))

{-Prajan -}
isPermutation' :: Eq a => [a] => [a] -> Bool   {- isPermutation [2,1,3] [1,2,3] : Reuslt is True-}
isPermutation' [] [] = True
isPermutation' (x:xs) ys | (length (x:xs) /= length ys) = error "List parameters not equal "
                        |  x `elem` ys = isPermutation xs (delArgInList (==x) ys)
                        |  otherwise = False

delArgInList ::  (a -> Bool) -> [a] -> [a]   {- this is delete argument in list compare (== 1) [1,2,3,4]    : Result is [1,3,4]-}
delArgInList p [] = []
delArgInList p (x:xs) | p x      = xs
                    | otherwise = x : delArgInList p xs

{-Jeroenn-}
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