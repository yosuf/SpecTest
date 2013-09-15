module Assignment1

where


is_triangle x y z = z < (x + y) && x < (z + y) && y < (z + x)
is_equalateral x y z = x == y && x == z

is_rect a b c =  (a^2) + (b^2) == (c^2)
is_rectangular x y z = or ([is_rect a b c | a <- [x, y, z],
                                            b <- [x, y, z],
                                            c <- [x, y, z]])
                                            
is_isosceles a b c = or [a1 == b1 && not (a1 == c1) | a1 <- [a, b, c],
                                                      b1 <- [a, b, c],
                                                      c1 <- [a, b, c]]
data Shape = NoTriangle | Equilateral
             | Isosceles | Rectangular | Other deriving (Eq,Show)
             
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | not (is_triangle a b c) = NoTriangle
                | is_equalateral a b c = Equilateral
                | is_rectangular a b c = Rectangular
                | is_isosceles a b c = Isosceles
                | otherwise = Other
                
-- test
assert :: Shape -> Shape -> Shape
assert expected actual  | expected == actual = expected
                        | otherwise = error "expected not the same as actual"

prop1 = assert (triangle 10 5 5)