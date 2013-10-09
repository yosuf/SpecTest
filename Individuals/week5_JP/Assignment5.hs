module Assignment5 where

import Data.List
import Week5Adapted
import RandomSudoku
import Assignment3

-- isPermutation from Week 3
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []     = True
isPermutation ys xs     | length ys /= length xs        = False
                        | (length.removeAll ys) xs == 0 = True
                        | otherwise = False
removeAll [] xs = xs
removeAll (y:ys) xs = removeAll ys (delete y xs)

-- property that a given list is a permutation of possible Sudoku values
isSudokuPerm :: [Int] -> Bool
isSudokuPerm = isPermutation values

-- gives all column valuations
columnValuations :: Sudoku -> [[Value]]
columnValuations s = [[s (y,x) | x <- positions] | y <- positions]

-- gives all line valuations
lineValuations :: Sudoku -> [[Value]]
lineValuations s =  [[s (y,x) | y <- positions] | x <- positions]

-- gives all block valuations
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

solve s = solveNs [(s, constraints s)]

oneSolution nodes = (length nodes) == 1

-- a Sudoku is minimal if it has one solution, and erasing each filled position of the Sudoku does not lead to a single solution
minimalProperty :: Sudoku -> Bool
minimalProperty s = (oneSolution $ solve s) &&
                    and [ (length $ solve $ eraseS s rc) /= 1 | rc <- filledPositions s]

--tests the solutions ([Node]) for the given property (Sudoku -> Bool)
test :: (Sudoku -> Bool) -> [Node] -> Bool
test prop nodes = and [prop $ fst n | n <- nodes]

doTenTests = doTest 10
doTest n = do   [r] <- rsolveNs [emptyN]
                p <- genProblem r
                showNode p
                let solutions = solveNs [p]
                putStrLn $ "Has mimimal property = " ++ (show $ minimalProperty $ fst p)
                putStrLn $ "Has permutation property? " ++ (show $ test valuationsPermutationProp solutions)
                if n > 0 then doTest (n-1) else return()
