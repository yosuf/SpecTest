module Fib where


nats = [0..10]

deran' = 1:0: zipWith (*) (tail nats) (zipWith (+) deran (tail deran))


deran :: Int -> [[Int]]
deran n = filter (\  xs -> all (\ m -> xs!!m /= m)) [n..n-1]
	(perms [0..n-1])