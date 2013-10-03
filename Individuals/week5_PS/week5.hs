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
-}


mergeSrtA :: Ord a => [a] => [a]
mergeSrtA = assert1 merge $ assert1 mergeSrtA



