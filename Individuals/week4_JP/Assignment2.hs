module Assignment2

where

import System.Random
import SetOrd

-- Last weeks code for random integers -->
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- Generates a random set with variable length up till 100
rndSet :: IO (Set Int)
rndSet = do x <- getRandomInt 100
            s <- rndSet' x
            return s

-- Generates a random set with a maximum length of N
rndSet' :: Int -> IO (Set Int)
rndSet' 0 = return (Set [])
rndSet' n = do  i <- getRandomInt (n*10)
                s <- rndSet' (n-1)
                return (insertSet i s)
