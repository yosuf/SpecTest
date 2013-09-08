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


-- spielerei
p b x | b       = [x]
      | not b   = []

findprimes l = [x | x <- l, prime0 x]

deduct b x | b      = x-1
           | not b  = x

findNprimes n (x:xs) | n == 0    = []
                     | otherwise = (p (prime0 x) x) ++ (findNprimes (deduct (prime0 x) n) xs)


findNprimes' n = findNprimes n [1..]

factorial [] = 1
factorial (x:xs) = x * factorial xs