
{- 1 
NoTriangle : Three numbers cannot form a triangle if the sum of any two is less then the third. eg 1 0 2, 2 7 3

-}
module Week2 
where 
import Week2Module

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving ( Eq, Show)

pSum :: Integer -> Integer -> Integer
pSum x y = (x ^ 2 + y ^ 2)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z = if ( (x + y < z) || (x + z < y) || (y + z) < x ) then NoTriangle
	             else if (x == y && x == z) then Equilateral
	             else if ( x == y || x == z || y == z) then Isosceles
	             else if ((pSum x y == z ^ 2 ) || (pSum x z == y ^ 2 ) || ( pSum y z == x ^ 2)) then Rectangular
	             else Other
	        
{-
triangle 1 0 2 : NoTriangle
triangle 1 1 1 : Equilateral
triangle 2 1 2 : Isosceles
triangle 3 4 5 : Rectangular
triangle 2 7 8 : Other

Indication of time spent : around 3 hours 

-}


{- 2 -}
contradiction :: Form -> Bool 
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = not (contradiction f)

entails :: Form -> Form -> Bool  {- Logical entailment -}
entails f1 f2 = tautology ( Impl f1 f2)

equiv :: Form -> Form -> Bool {- Logical Equivalence -}
equiv f1 f2 = tautology ( Equiv f1 f2)


