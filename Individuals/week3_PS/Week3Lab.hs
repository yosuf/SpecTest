module Week3Lab where
import Data.List
import Data.Char
import System.Random




{- 3 Generator for random integer lists -}


genIntList ::  IO [Integer]
genIntList = [312121] 


{-
generateNoTri :: [Integer] -> [[Integer]]
generateNoTri xs = [[x,y,z] | x <- xs, y <- [0..x], z <- [x*2..x*2] ]

-}