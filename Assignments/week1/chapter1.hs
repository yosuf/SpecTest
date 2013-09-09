-- Given functions
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

divides :: Integer -> Integer -> Bool 
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n = n
        | otherwise = ldf (k+1) n
        
prime0 :: Integer -> Bool
prime0 n  | n < 1     = error "not a positive integer"
          | n == 1    = False
          | otherwise = ld n == n
          
{- 1.4 
  It would not make a difference. It would save one recursion of the function.
-}
{- 1.6
  rem :: Integral -> Integral -> Integral
-}
{- 1.7
  divides 5 :: Integer -> Bool
  The type of divides 5 is a function that takes one argument (the ommited argument) and 'produces' a bool
  divides 5 7 :: Bool
  Here all arguments are given, so their types can be resolved. The resulting type is a bool.
-}

{- 1.1

2 * 3 + 1
2 * (3 + 1)
1 + 2 - 1 * 3
4 / 3 * 3 + 1 
3^2 + 5
-}

-- 1.9
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- 1.10
-- Found two solutions, probably the second (removeFst') is the most functional one
           
removeFst :: Int -> [Int] -> [Int]
removeFst m [] = []
removeFst m (x:xs) = if m == x then xs else x:(removeFst m xs)

removeFst' :: Int -> [Int] -> [Int]
removeFst' m [] = []
removeFst' m (x:xs) | m == x = xs
                    | otherwise = x:(removeFst m xs)
                    
-- 1.11
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

-- 1.13
-- Again two solutions. Solution 2 (count') is probably most readable
count :: Char -> String -> Int
count a [] = 0
count a (c:cs) = let 
                  x = if a == c then 1 else 0 
                in x + (count a cs)

count' :: Char -> String -> Int                
count' a [] = 0
count' a (c:cs) | a == c    = 1 + (count' a cs)
                | otherwise = (count' a cs)
                
-- 1.14
blowup_ :: Int -> String -> String
blowup_ _ [] = []
blowup_ n (c:cs) = (take n (repeat c)) ++ (blowup_ (n+1) cs)

blowup :: String -> String
blowup x = blowup_ 1 x


-- 1.15
-- Could I do this in a more concise way?

mnmString :: [String] -> String
mnmString [] = error "empty list"
mnmString [x] = x
mnmString (x:xs) = min x (mnmString xs)

removeFstString :: String -> [String] -> [String]
removeFstString f [] = []
removeFstString f (s:ss)  | f == s    = ss
                          | otherwise = s:(removeFstString f ss)

srtString :: [String] -> [String]
srtString [] = []
srtString ss = mins:(srtString(removeFstString mins ss)) where mins = mnmString ss

-- 1.17

substring :: String -> String -> Bool
substring xs ys | prefix xs ys        = True
                | prefix xs (tail ys) = True
                | otherwise           = False
                
{- 1.18
  1. lines "test" :: [String]
  2.-
  3. zip [True,False] ["a","b"] :: [(Bool, [Char])]
  4. -
  5. (True ==) :: Bool -> Bool

-}

{- 1.19
  1.  head :: [a] -> a
      returns the first element from the list
      head [1,2,3,4]        (1)
      
  2.  last :: [a] -> a
      returns the last element from the list
      last [1,2,3,4]        (4)
      
  3.  init :: [a] -> [a]
      returns the list without the last element
      init [1,2,3,4]        ([1,2,3])
      
  4.  fst :: (a, b) -> a
      returns the first component from the pair/tuple
      fst ("abc","def")     ("abc")
      
  5.  (++) :: [a] -> [a] -> [a]
      concatenation of two lists. 
      (++) [1,2] [3,4]      ([1,2,3,4])

      
  6.  flip :: (a -> b -> c) -> b -> a -> c  
      takes a function that takes a and b and produces c
      takes arguments b and a and produces c
      seems that flip calls the function with the arguments reversed
      flip (-) 6 2          (-4)

      
  7.  flip (++) :: [a] -> [a] -> [a]
      here types are narrowed down as (++) operates on one type only
      
-}

-- 1.20
lengths :: [[a]] -> [Int]
lengths xs = map length xs

-- 1.21
sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)

-- 1.24
-- !!PLEASE ANYONE ADD SOLUTION HERE!!
