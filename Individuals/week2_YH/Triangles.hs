module Triangles where

import Test.QuickCheck
import Data.List

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = triangle' (sort [x,y,z] )

-- pre-condition: Sorted list of integers which has max 3 elements. 
triangle' :: [Integer] -> Shape
triangle' (x:y:z:rest)
	| length rest /= 0 = NoTriangle
	| x<0 || y<0 || z<0 = NoTriangle
	| x + y <= z = NoTriangle
	| x == y && y == z = Equilateral
	| x == y || x == z || y == z = Isosceles
	| x^2 + y^2 == z^2 = Rectangular
	| otherwise = Other



generateRect :: [Integer] -> [[Integer]]
generateRect xs = [[x,y,z] | x <- xs, y <- [1..x-1], z <- [1..x-1] , x^2 == y^2 + z^2 ]

testRect :: Integer -> [Bool]
testRect nrOfTestCases = [ triangle x y z == Rectangular | x:y:z:testSet <- generateRect[1..nrOfTestCases] ]

--generateRect:: Integer -> Bool
--generateRect x  = (div x 2)^2 + (div x 2)^2 == x^2  then True  else False


--rectangularNr:: [Integer] -> [Integer]
--rectangularNr	[] = []
--rectangularNr (x:xs) = if ((div x 2)^2 + (div x 2)^2 == x^2) then x:(rectangularNr xs) else rectangularNr xs






{- Report
Time spent: ~2 hours
Tested: by running examples from ghci.
-}

test_Triangle_No x y z = triangle x y z == NoTriangle
	where types = (x, y, z )

-- successful
prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Integer]

-- unsuccessful
prop_RevId xs = reverse xs == xs
  where types = xs::[Int]