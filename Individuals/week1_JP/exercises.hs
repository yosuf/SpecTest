-- Given non-excercise functions
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list" 
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

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
