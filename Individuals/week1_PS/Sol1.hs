{- Chapter 1 and 2 -}


{- | 1.1      precedence :         ( ), ^, /  , * , + , -             [ BODMAS ]            -}

{- 1.2 -}

divides :: Integer -> Integer -> Bool

divides d n = rem n d == 0

{- 1.4 -}
ldf k n 	| divides k n	= k
            | k ^ 2 >= n	= n 
            | otherwise   	= ldf ( k + 1 ) n 

{- Ans : doing k ^ 2 >= n doesn't make any difference becuase the first guarded equation will always satisfy prior to moving to the second guraded equation.
              eg : ldf 4 16 will satisfy first condition and return 4. If we try to apply the given condition then also the program will never reach to the second guarded condition               as the fist one will already be a satisfied one.  -}

{- 1.5 -}
ld n 	= ldf 2 n    {- this needs to be placed ; ldf 2 n means the number n which are divisble by 2 -}

prime0 n 	| n < 1	= error "not a positive integer"
	| n == 1 	= False 
	| otherwise = ld n == n

{- 1.6  -}
{- rem :: Integer -> Integer -> Integer -}

{- 1.7 -}
{-   :t divides 4          return divides 4 :: Integer -> Bool ; This is becuase the first argument as integer is already loaded -}
{-   :t divides 2 3       returns divides 2 3 :: Bool ; this is because input arguments are already loaded and is expecting for a boolean result -}


{- Example 1.8 -}
minInt :: [Int] -> Int
minInt [] = error "empty list"
minInt [x] = x
minInt (x:xs) = min x ( minInt xs ) 


{- 1.9 -}
mxmInt  :: [Int] -> Int
mxmInt [] = error "Empty list"
mxmInt [x] = x
mxmInt (x : xs) =  max x (mxmInt xs) 

{- 1.10 -}
removeFst :: Int -> [Int]   -> [Int]
removeFst x [] = []
removeFst  m (x : xs) 	| m == x 	= xs {- Only remove first occurence of m -}
					{-	| m == x 	= removeFst x xs   ; if need to remove all occurence of m -}
						| otherwise =  x:removeFst m xs




{-Example 1.11 -}
strInts :: [Int] -> [Int]
strInts [] = []
strInts xs = m : (strInts (removeFst m xs)) where m = minInt xs {- No x:xs as parameter -}


{- Example 1.12 -}
average :: [Int] -> Rational
average [] = error "empty list"
average xs = toRational (sum xs) /  toRational (length xs)

pSum :: [Int] -> Int
pSum [] = 0
pSum (x:xs) = x + pSum xs

pLength :: [a] -> Int  {- Contains variable a ; type scheme  rather than a type -}
pLength [] = 0
pLength (x:xs) = 1 +  pLength xs 

{- 1.13 -}
count :: Char -> String -> Int          {- count 'a' "prajan"   : Ans 2-}
count m [] = 0
count m (x:xs) 	| m == x 	= 1 + count m xs {- find input m in list xs -}
				| otherwise = count m xs


{- 1.14 -}				
{-transform :: String -> String
transform [] = []
transform (x:xs) 	=  2 * [x] ++  transform xs

-}

{- 1.15 -}


{- Example 1.16 -}

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs)(y:ys) = (x==y) && prefix xs ys   {- prefix "pra" "prajan"  ; resutl is True -}




{- 1.17 -}
{-
substring :: String -> String -> Bool 
substring [] ys = 

substring (x:xs) (y:ys) = 
-}

{-   1.18 
[String]  = :t "Prajan"
(Bool, String) = :t (True, "Prajan")
[(Bool,String)] = :t [(True, "praj"), (False, "jan")]
{[Bool], String} = :t ([True, False], "Prajan")
Bool -> Bool = :t not   ; it negates boolean value means changing True to Flase of viceversa

-}

{-  1.19

head = [a] -> a  head [1,2,3] -> 1
last = [a] -> a
init = [a] -> [a]
fst  = (a,b) -> a
(++) = [a] -> [a] -> [a]   example [1,2] ++ [3,5] -> [1,2,3,5]
flip = (a -> b -> c) -> b -> a -> c
flip (++) = [a] -> [a] -> [a]

-}


{- The Prime Factorization Algorithm -}
factors :: Integer-> [Integer]
factors n 	| n < 1 	= error "argument not positive"
			| n == 1 	= []
			| otherwise =   p : factors (div n p)  where p = ld n 


{-1.20-}			
lengths :: [[a]] -> [Int]
lengths x = map pLength x
{- lengths [[1],[1,2,3,4],[1,2,3,4,7]]   => [1,4,5]-}


{-1.21-}
sumLengths :: [[Int]] -> [Int]
sumLengths x = map pSum x   {- pSum is a function for map that takes lists of list ; sumLengths [[1,2],[2,4]] = [3,6]-}
{- map f (x:xs) = f x : map f xs =>  
   map pSum [[1,2],[3,4],[5,6]] = pSum [1,2] : map pSum [[3,4],[5,6]] -}


{- Filters -}
pFilter :: (a -> Bool) -> [a] -> [a]    {- filter (>3) [1..5] = ans is [4,5]-}
pFilter p [] = []
pFilter p (x:xs) 	| p x 		= x : pFilter p xs   {- p x : this represents a condition -}
					| otherwise = pFilter p xs

{- Example 1.23 -}					

ldp :: Integer -> Integer
{-ldp n = ldpf primes1 n    {- ldp 9 has ans 3 ; ldp 5 has 5 -}-}   {- Comment line 173 -}

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n 	| rem n p == 0 	= p
				| p ^2 > n 		= n 
				| otherwise		= ldpf ps n 

primes1 :: [Integer]				
primes1 = 2 : filter prime [3..]

prime :: Integer -> Bool
prime n 	| n < 1 	= error "not a positive number"
 			| n == 1 	= False
 			| otherwise = ldp n == n 

{- 1.24 -}
ldp = ldpf primes1    {- comment line 157 -}