module Assignment3

where

import SetOrd

-- intersection
intersect :: (Ord a) => Set a -> Set a -> Set a
intersect _     (Set [])        = emptySet
intersect s1    (Set (x:xs))    | inSet x s1    = insertSet x (intersect s1 (Set xs))
                                | otherwise     = intersect s1 (Set xs)

-- union
union :: (Ord a) => Set a -> Set a -> Set a
union s1    (Set [])        = s1
union s1    (Set (x:xs))    = insertSet x (union s1 (Set xs))

-- diff (x in s2, x !in s1)
diff :: (Ord a) => Set a -> Set a -> Set a
diff s1     (Set [])        = emptySet
diff s1     (Set (x:xs))    | inSet x s1    =   diff s1 (Set xs)
                            | otherwise     =   insertSet x (diff s1 (Set xs))

s1 = Set [1,2,3,4]
s2 = Set [3,4,5,6]