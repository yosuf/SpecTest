module Lab2
where

import Data.List



data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

-- Yosuf
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

  
-- According to input, this method generates the possible rectangulars from a given range
-- exampe: generateRect [1..50] will result in a list of lists [[3,4,5], ...] 
-- which can then be used to chech the program
generateRect :: [Integer] -> [[Integer]]
generateRect xs = [[x,y,z] | x <- xs, y <- [1..x-1], z <- [1..x-1] , x^2 == y^2 + z^2 ]

-- This is the ultimate test case for the rectangular triangles ;)
-- How does it work? 
-- Invoke it with a numer. it will call the generateRect to generate valid rect triangles.
-- It then 'asserts' theses to the triangle method and notes whether the outcome is Rectangular
testRect :: Integer -> [Bool]
testRect nrOfTestCases = [ triangle x y z == Rectangular | x:y:z:testSet <- generateRect[1..nrOfTestCases] ]



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

triangle'' a b c  | not (is_triangle a b c) = NoTriangle
                | is_equalateral a b c = Equilateral
                | is_rectangular a b c = Rectangular
                | is_isosceles a b c = Isosceles
                | otherwise = Other
                
-- test
assert :: Shape -> Shape -> Shape
assert expected actual  | expected == actual = expected
                        | otherwise = error "expected not the same as actual"

prop1 = assert (triangle'' 10 5 5)