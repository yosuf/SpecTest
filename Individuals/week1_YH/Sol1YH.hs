module Sol1YH where

import Data.List
import Data.Maybe
import qualified GS


divides :: Integer -> Integer -> Bool
divides d n = rem n d ==0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer 
ldf k n	| divides k n = k
		| k^2 > n     = n
		| otherwise   = ldf (k+1) n


prime0 :: Integer -> Bool
prime0 n 	| n < 1		= error "Non positive integer. Not applicable to prime numbers."
			| n == 1	= False
			| otherwise	= ld n == n


mnmInt :: [Int] -> Int
mnmInt [] = error "Ah :P Empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)


max' :: Integer -> Integer -> Integer
max' x y | y > x = y
		 | otherwise = x 


-- remove the first occurence of an integer m from a list of ints. if m does not occur in the list, it remains unchanged.
removeFst :: Int -> [Int] -> [Int]
removeFst m xs 	| length xs < 1 	= xs
				| not (elem m xs)	= xs
				| otherwise = (fst (splitAt (fromJust (elemIndex m xs)) xs)) ++ tail (snd (splitAt (fromJust (elemIndex m xs)) xs))


srtInts' :: [Int] -> [Int]
srtInts' [] = []
--srtInts' xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs
srtInts' xs = let m = mnmInt xs in m : (srtInts' (removeFst m xs))


