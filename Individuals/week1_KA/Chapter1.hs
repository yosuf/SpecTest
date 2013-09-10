-- Exercise 1.9
maxInt :: [Int] -> Int
maxInt [] = error "An empty list can't have a maximum value"
maxInt [x] = x
maxInt (x:xs) = max' x (maxInt xs)

max' :: Int -> Int -> Int
max' x y | x >= y = x
	     | otherwise = y

-- Exercise 1.10
removeFst :: Int -> [Int] -> [Int]
removeFst m [] = []
removeFst m (x:xs) | x == m = xs
				   | otherwise = removeFst m xs

removeAll :: Int -> [Int] -> [Int]
removeAll m [] = []
removeAll m (x:xs) = [x| x <- (x:xs), x /= m]

-- Exercise 1.13
count :: Char -> String -> Int
count c [] = 0
count c (x:xs) | x == c = 1 + count c xs
			   | otherwise = count c xs

-- Exercise 1.14
blowup' :: Int -> String -> String
blowup' n [] = []
blowup' n (x:xs) = replicate n x ++ blowup' (n + 1) xs

blowup :: String -> String
blowup x = blowup' 1 x