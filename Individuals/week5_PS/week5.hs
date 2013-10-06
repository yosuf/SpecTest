module Week5
where
import LibLecture.Week5Lecture
import LibLecture.RandomSodoku


{-1. Merge Sort. Find suitable assertion, and write an assertive version -}

mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

{-
assert1 :: (a -> b -> Bool) -> (a -> b) -> a -> b 
assert1 p f x = if p x (f x) then f x 
                else error "assert1"

assert2 ::  (a -> b -> c -> Bool) 
             -> (a -> b -> c) -> a -> b -> c
assert2 p f x y = 
  if p x y (f x y) then f x y
  else error "assert2"
mergeA :: Ord a => [a] -> [a] -> [a]
mergeA = assert2 sortedProp 
            $ assert2 sublistProp merge

-}

sortList :: Ord a => [a] -> [a] -> Bool    {- Assertion Decided onthe fact that assert1 first input is (a -> b-> Bool) -}
sortList xs ys = sorted xs ==> sorted ys

mergeSrtA :: Ord a => [a] => [a]
mergeSrtA = assert1 sortList mergeSrt   {- sorList function has two inputs and return a bool -}



{-2. another approach to merge sort via splitting the list and 
Steps:
1. take the list. mergeSrtUsingSplit
2. split the list into 2 equal parts by using defined split function
3. recursively sort them 
4. finally merge them. 

(2,3,4) = 2 : (3,4) = x:xs 
-}


split :: [a] -> ([a],[a])
split xs = let
			n = (length xs) `div` 2
			in (take n xs, drop n xs)

mergeSrtUsingSplit :: Ord a => [a] -> [a]
mergeSrtUsingSplit []= []
mergeSrtUsingSplit [x]= [x]
mergeSrtUsingSplit (x:xs)  = let (a,b) = split (x:xs) in merge (mergeSrtUsingSplit a)  (mergeSrtUsingSplit b)

{- Suitable assertion and assertive version -}
mergeSrtUsingSplitA :: Ord a => [a] -> [a]
mergeSrtUsingSplitA = assert1 sortList mergeSrt



{-3.  Formlaise extra constraint for this Sudoku : solution in LibLecture\week5Lecture.hs
- create new showRow for internal matrix
- update showGrid for the display
- create a new blcoks' to hold new block of 2..4 and 6..8
- create new bl' to hold the value needed
- create subGrid' for internal grid
- create freeInSubgrid' 
- Use the above for freeAtPos
- Create subgridInjective' 
- Add them to consistent
- Create sameblock' .
- Add this to prune
- Make example6 for the problem mentioned in the pdf
-}


{-4. exercise 3 and RandomSudoku.hs to  generate NRC-Handelsblad sudoku problems-}
--main :: IO ()
genNRC = do
		   [r] <- rsolveNs [emptyN]
		   showNode r
		   s <- genProblem r
		   showNode s
		   solveShowNs [s]


{-5 Test the programs
  Property to test is whether the generated sudoku problems are minimal -}
