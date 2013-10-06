module Assignment2

where

import Data.List
import Week5

split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2 in (take n xs, drop n xs)