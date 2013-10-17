
import Week6
-- Assignment 1

exM' :: Integer -> Integer -> Integer -> Integer
exM' x k n = (mul $ factorize k $ collect x k n) `mod` n


collect x k n       = (x `mod` n,1) : collect' x k n 2
collect' x k n k2   | k2 <= k    = let a = x^2 `mod` n in (a, k2) : collect' a k n (k2*2)
                    | otherwise  =  []

findlargest k [] = (0,0)
findlargest k (x:xs)  | (snd x) <= k = let a = (findlargest k xs) in if a /= (0,0) then a else x
                      | otherwise = (0,0)

factorize k l = let a = findlargest k l in
                    if (snd a) > 1 then a : factorize (k-(snd a)) l else [a]

mul [] = 1
mul (x:xs) = fst x * mul xs
                   -- [9,28,42,15,13]
                   -- 23343 24333333 2534