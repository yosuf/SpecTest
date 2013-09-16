-- 1
data Shape = NoTriangle | Equilateral | 
			 Isosceles  | Rectangular | 
			 Other deriving (Eq,Show) 

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | not (isTriangle a b c) = NoTriangle
               | isEquilateral a b c = Equilateral
			   | isIsosceles a b c = Isosceles
			   | isRectangular a b c = Rectangular
			   | otherwise = Other

isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = a < b + c && b < a + c && c < a + b  

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = isTriangle a b c && a == b && c == b

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = isTriangle a b c && 
					not (isEquilateral a b c) &&
				    (a == b || a == c || b == c)

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = isTriangle a b c  &&
                      (a^2 == b^2 + c^2 || 
					   b^2 == a^2 + c^2 ||
					   c^2 == a^2 + b^2)
					   
testTriangle :: Shape -> (Integer -> Integer -> Integer -> Bool) -> Bool
testTriangle shape f = and [ (triangle a b c) == shape | c <- [1..10] , b <- [1.. c], a <-[1.. b], (f a b c)]					   
--testIsEquilateral = and [ (triangle a b c) == Equilateral | c <- [1..10] , b <- [1.. c], a <-[1.. b], a < b + c && b < a + c && c < a + b &&(a == b && c == b) ] 
testIsEquilateral = testTriangle Equilateral (\ a b c -> a < b + c && b < a + c && c < a + b &&(a == b && c == b))
testIsIsosceles = testTriangle Isosceles (\ a b c -> a < b + c && b < a + c && c < a + b && (a == b || a == c || b == c) && not (a == b && c == b))
testIsRectangular = testTriangle Rectangular (\ a b c -> a < b + c && b < a + c && c < a + b && (a^2 == b^2 + c^2 || b^2 == a^2 + c^2 || c^2 == a^2 + b^2))