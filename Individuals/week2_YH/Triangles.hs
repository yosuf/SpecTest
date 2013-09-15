module Triangles where


data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
	| x<1 || y<1 || z<1 = NoTriangle   -- like: 0 1 1
	| x == y && y == z = Equilateral		-- like: 5 5 5 
	| x == y || x == z || y == z = Isosceles  -- like: 2 2 6
	| (min x y)^2 + (min y z)^2 == (max (max x y) z)^2 = Rectangular -- like: 3 4 5
	| otherwise = Other -- like anything else




{- Report
Time spent: ~2 hours
Tested: by running examples from ghci.
-}