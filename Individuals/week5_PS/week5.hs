module Week5
where
import 	LibLecture.Week5Lecture

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

sortList :: Ord a => [a] -> [a] -> Bool    {- Decided onthe fact that assert1 first input is (a -> b-> Bool) -}
sortList xs ys = sorted xs ==> sorted ys

mergeSrtA :: Ord a => [a] => [a]
mergeSrtA = assert1 sortList mergeSrt   {- sorList function has two inputs and return a bool -}



