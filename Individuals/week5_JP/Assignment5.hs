module Assignment5 where

import Data.List
import Week5Adapted
import Assignment3

-- isPermutation from Week 3
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []     = True
isPermutation ys xs     | length ys /= length xs        = False
                        | (length.removeAll ys) xs == 0 = True
                        | otherwise = False
removeAll [] xs = xs
removeAll (y:ys) xs = removeAll ys (delete y xs)


isSudokuPerm :: [Int] -> Bool
isSudokuPerm = isPermutation values

-- gives all column valuations
columnValuations :: Sudoku -> [[Value]]
columnValuations s = [[s (y,x) | x <- positions] | y <- positions]

-- gives all line valuations
lineValuations :: Sudoku -> [[Value]]
lineValuations s =  [[s (y,x) | y <- positions] | x <- positions]

-- gives all block valuation
blockValuations :: [[Int]] -> Sudoku -> [[Value]]
blockValuations blocks s = [[s (y,x) | x <- xs, y <- ys] | xs <- blocks, ys <- blocks]

-- gives all standard block valuations
standardBlockValuations :: Sudoku -> [[Value]]
standardBlockValuations = blockValuations blocks

-- gives all NRC block valuations
nrcValuations :: Sudoku -> [[Value]]
nrcValuations = blockValuations nrcblocks

-- collects all valuations
collectValuations :: Sudoku -> [[Value]]
collectValuations s = (lineValuations s) ++ (columnValuations s) ++ (standardBlockValuations s) ++ (nrcValuations s)

-- property that holds when all valuations of the Sudoku are permutations of [1..9]
valuationsPermutationProp :: Sudoku -> Bool
valuationsPermutationProp s = and [isSudokuPerm valuation | valuation <- collectValuations s]


snrc = (fst $ (solveNs (initNode nrc)) !! 0)