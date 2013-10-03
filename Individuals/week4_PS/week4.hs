

module Week4
where 
import Data.List 
import LibLecture.Week3Techniques
import LibLecture.Week4SetOrd
    
{-ps: Updated version of function inSet as this contains sort s instead of only s which was giving false in case of inSet 1 (Set [2,1,3])-}
inSortedSet  :: (Ord a) => a -> Set a -> Bool  
inSortedSet x (Set s) = elem x (takeWhile (<= x) (sort s)) 


{-2 Implement random data generator for the datatype Set Int-}


genRandomSet :: Int -> IO (Set Int)
genRandomSet n = genRandomSet' n emptySet

genRandomSet' :: Int -> Set Int -> IO (Set Int)
genRandomSet' n (Set s) | length s /= n =do 
											a <- getRandomInt n
											Set xs <- genRandomSet' n (insertSet a (Set s))
										 	return (Set xs)
					  	| otherwise = return (Set s)



{-3 Set Intersection , Set Union , Set Difference -}
{-Set Intersection
   A = {1,2,3}  B = {3,4,5,2 }   A intersection B = {2,3}
- take first element of A and compare wth all elements of B. if this element exists then keep it in a set else not 

Test cases :
- if the final set is a subset of both input set 
- if the final set is empty then they do not intersect at all.
- If any one elements is in the final list then they do intersect.


-Set Union
   A = {1,2,3} B = {2,4,5,6}  A union B = {1,2,3,4,5,6}
- Get first element of A and store it in a set 
- if all elements of A are finished move all elements of B also in the same set

Test cases:
- if the final set is emptyt then they have no unions. Also means that the input set are also empty.
- if at leat one element then they are union. 	

-Differenec Set
   A = {1,2,3} B = {1,3,5}  A difference B = {2}
- Get first element of A and compare it with Set B. If it exists then leave that element and move with the next one.

Test cases:
- if the final set is empty then sets have no difference at all.
- At least one element means that they do have a difference 	

-}


intersectionSet :: (Ord a) => Set a => Set a => Set a
intersectionSet (Set []) set2 = Set []
intersectionSet (Set (x:xs)) set2 	| inSortedSet x (set2) = insertSet x (intersectionSet (Set xs) set2)
									| otherwise = intersectionSet(Set xs) set2

unionMySet :: (Ord a) => Set a => Set a => Set a
unionMySet (Set []) set2 = set2
unionMySet (Set (x:xs)) set2 = insertSet x (unionMySet (Set xs) set2)

differenceSet :: (Ord a) => Set a => Set a => Set a 
differenceSet (Set []) set2 = Set []
differenceSet (Set (x:xs)) (set2) | inSortedSet x (set2) = differenceSet (Set xs) set2
   							      | otherwise = insertSet x (differenceSet (Set xs) set2)

{-
testIntersectionSet :: Int -> IO (Set Int)
--testIntersectionSet n | length(intersectionSet (Set [1,2,3]) (Set 2,3,4))/= length(emptySet) = print ("Intersected")
testIntersectionSet n = do 
							Set xs <- genRandomSet n
							Set ys <- genRandomSet n 
							tst (Set xs) 

							--return(Set xs)
							--return(Set ys)

tst :: Set a -> Set a -> IO ()						
tst xs ys | subSet (intersectionSet (Set xs)(Set ys)) Set xs && subSet (intersectionSet (Set xs)(Set ys)) Set ys = print (show "test")
--testIntersectionSet n | subSet (Set [1,2])(Set [1,2,3]) && subSet (Set [1,2]) (Set (sort[1,3,2,4])) = print (show "test")

-}


{-4 Implement binary relations as list of pairs 
Transitive Closure : 
  trClos [(1,2),(2,3),(3,4)]   =>   [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)] : trClos is either itself or to the pair

-}

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r , (w,z) <- s , y == w]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos(x:xs) = ([x] ++ ([x] @@ (trClos xs))) ++ (trClos xs)



