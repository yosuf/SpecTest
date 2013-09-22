module Week3

where 

import Data.List

-- exercise 3
genIntList' :: Int -> Int -> IO [Int]
genIntList' _ 0 = return []
genIntList' d n = do
                f <- getRandomInt d
                fs <- genIntList' d (n-1)
                return (f:fs)

genIntList :: IO [Int]
genIntList = genIntList' 100 20



-- exercise 4
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys | length xs /= length ys = False
                    | xs == ys = True
                    | otherwise = or (map (==xs) (permutations ys))