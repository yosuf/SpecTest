module Assignments where

import Data.List

{- Yosuf Haydary
Time taken: 1 hr
-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs xy = if (length xs) /= (length xy) then  False 
	                  else and [and [elem s xy, elem y xs] | s <- xs, y <- xy ]

