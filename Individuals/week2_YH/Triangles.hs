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



-- Testing No Triangle
generateNoTri :: [Integer] -> [[Integer]]
generateNoTri xs = [[x,y,z] | x <- xs, y <- [0..x], z <- [x*2..x*2] ]

testNoTri :: Integer -> [Bool]
testNoTri nrOfTestCases = [ triangle x y z == NoTriangle | x:y:z:testSet <- generateNoTri[1..nrOfTestCases] ]


-- Tesging Equilateral
generateEqui :: [Integer] -> [[Integer]]
generateEqui xs = [[x,x,x] | x <- xs ]

testEqui :: Integer -> [Bool]
testEqui nrOfTestCases = [ triangle x y z == Equilateral | x:y:z:testSet <- generateEqui[1..nrOfTestCases] ]


-- Tesging Equilateral
generateIsos :: [Integer] -> [[Integer]]
generateIsos xs = [[x,y,z] | x <- xs, y <- [1..x],  z <- [1..x], y==x, z/=x ]

testIsos :: Integer -> [Bool]
testIsos nrOfTestCases = [ triangle x y z == Isosceles | x:y:z:testSet <- generateIsos[1..nrOfTestCases] ]

-- Testing Other
generateOther :: [Integer] -> [[Integer]]
generateOther xs = [[x,y,z] | x <- xs, y <- [1..x],  z <- [x..x*2], y/=x, y/=z, z/=x, x^2 /= y^2 + z^2, z < (x+y) ]

testOther :: Integer -> [Bool]
testOther nrOfTestCases = [ triangle x y z == Other | x:y:z:testSet <- generateOther[1..nrOfTestCases] ]


-- Testing Rectangular
testRect :: Integer -> [Bool]
testRect nrOfTestCases = [ triangle x y z == Rectangular | x:y:z:testSet <- generateRect[1..nrOfTestCases] ]

generateRect :: [Integer] -> [[Integer]]
generateRect xs = [[x,y,z] | x <- xs, y <- [1..x-1], z <- [1..x-1] , x^2 == y^2 + z^2 ]


{- Report
Time spent: ~5 hours
Tested by writing test functions which generates different triangles for different forms
-}

test_Triangle_No x y z = triangle x y z == NoTriangle
	where types = (x, y, z )

-- successful
prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Integer]

-- unsuccessful
prop_RevId xs = reverse xs == xs
  where types = xs::[Int]