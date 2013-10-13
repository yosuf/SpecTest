module Primes where

first = 1299827 --7907
second = 1299821 --7919 
prod = first * second


myGcd:: Integer -> (Integer, Integer)
myGcd pr = myGcd' pr 2 2



myGcd' :: Integer -> Integer -> Integer -> (Integer, Integer)
myGcd' pr fir sec = if (fir * sec) + fir + sec + 1 == pr then (fir+1, sec+1) 
	else myGcd' pr (fir+1) (sec+1)


3 * 5 = 15 -1 =14
2 * 4 + 2 + 4 = 14 


7 * 5 = 35 -1 =34
6 * 4 + 6 + 4 = 34


7 * 7 = 49 -1 = 48
6 * 6 + 6 + 6 =  48

--take a max pr such that pr^2 still remains smaller than the x