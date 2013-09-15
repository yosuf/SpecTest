module Lab2
where

-- Question 1.
-- Triangles
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
	| x<1 || y<1 || z<1 = NoTriangle   -- like: 0 1 1
	| x == y && y == z = Equilateral		-- like: 5 5 5 
	| x == y || x == z || y == z = Isosceles  -- like: 2 2 6
	| (min x y)^2 + (min y z)^2 == (max (max x y) z)^2 = Rectangular -- like: 3 4 5
	| otherwise = Other -- like anything else

  
 

-- Jeroen:

is_triangle x y z = z < (x + y) && x < (z + y) && y < (z + x)
is_equalateral x y z = x == y && x == z

is_rect a b c =  (a^2) + (b^2) == (c^2)
is_rectangular x y z = or ([is_rect a b c | a <- [x, y, z],
                                            b <- [x, y, z],
                                            c <- [x, y, z]])
                                            
is_isosceles a b c = or [a1 == b1 && not (a1 == c1) | a1 <- [a, b, c],
                                                      b1 <- [a, b, c],
                                                      c1 <- [a, b, c]]

triangle' a b c  | not (is_triangle a b c) = NoTriangle
                | is_equalateral a b c = Equilateral
                | is_rectangular a b c = Rectangular
                | is_isosceles a b c = Isosceles
                | otherwise = Other
                
-- test
assert :: Shape -> Shape -> Shape
assert expected actual  | expected == actual = expected
                        | otherwise = error "expected not the same as actual"

prop1 = assert (triangle' 10 5 5)