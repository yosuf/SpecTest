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