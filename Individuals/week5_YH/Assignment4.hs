module Assignment4 where

import RandomSudoku
import Week5


--main :: IO ()
main = 
	do 
	r <- genRandomSudoku
	s <- genProblem r
	showNode s

