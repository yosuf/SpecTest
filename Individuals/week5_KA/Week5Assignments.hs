module Lab5

where

import Data.List
import Week5

-- assign 1
mergeSrt :: Ord a => [a] -> [a]
mergeSrt [] = []
mergeSrt (x:xs) = merge [x] (mergeSrt xs)

mergeSrtAssert1 :: ([a] -> Bool) -> ([a] -> [a]) -> [a] -> [a]
mergeSrtAssert1 postCond f xs = if postCond (f xs) then f xs
						  else error "mergeSrtAssert1"
						  
assertiveMergeSrt :: Ord a => [a] -> [a]
assertiveMergeSrt xs = mergeSrtAssert1 sorted mergeSrt xs

-- assign 2
split :: [a] -> ([a],[a])
split xs = let n = (length xs) `div` 2
		   in (take n xs, drop n xs)

splitMergeSrt :: Ord a => [a] -> [a]
splitMergeSrt [] = []
splitMergeSrt [x] = [x]
splitMergeSrt xs = 
				let                    
					fstP = (fst (split xs))
					sndP = (snd (split xs))					
				in (merge (splitMergeSrt fstP) (splitMergeSrt sndP))
 
assertiveSplitMergeSrt :: Ord a => [a] -> [a]
assertiveSplitMergeSrt xs = mergeSrtAssert1 sorted splitMergeSrt xs

-- assign 3
-- 
{- NRC requirements
   The members of every subgrid [i; j] with i; j ranging over 1::3, 4::6 and
   7::9 should be all different And also the members of every subgrid [i; j] with i; j ranging over 
   i <- [2..4], j <- [2..4] and i <- [2..4], j <- [6..8] and 
   i <- [6..8], j <- [2..4] and i <- [6..8], j <- [6..8]
   should be all different
-}

nrcConsistent s = consistent s && 
				(and [ subgridInjective s (r,c) | 
                       r <- [2,6], c <- [2,6]] )

nrcSubGrids = fstNrcSubGrid
             `union` sndNrcSubGrid
			 `union` trdNrcSubGrid
			 `union` frtNrcSubGrid
			 
fstNrcSubGrid = [(i,j) |  i <- [2..4], j <- [2..4]]
sndNrcSubGrid = [(i,j) |  i <- [2..4], j <- [6..8]]
trdNrcSubGrid = [(i,j) |  i <- [6..8], j <- [2..4]]
frtNrcSubGrid = [(i,j) |  i <- [6..8], j <- [6..8]]

getNrcSubGrid (r,c) | elem (r,c) fstNrcSubGrid = (2,2)
                      | elem (r,c) sndNrcSubGrid = (2,6)
					  | elem (r,c) trdNrcSubGrid = (6,2)
					  | elem (r,c) frtNrcSubGrid = (6,6)
					  | otherwise = error "Not in NRC subgrids"
					  
freeAtPosNrc :: Sudoku -> (Row,Column) -> [Value]
freeAtPosNrc s (r,c) = if elem (r,c) nrcSubGrids then                         
						 (freeInRow s r) 
                         `intersect` (freeInColumn s c) 
                         `intersect` (freeInSubgrid s (r,c)) 
                         `intersect` (freeInSubgrid s (getNrcSubGrid (r,c)))  
                       else
                        (freeInRow s r) 
                        `intersect` (freeInColumn s c) 
                        `intersect` (freeInSubgrid s (r,c)) 

nrcConstraints :: Sudoku -> [Constraint] 
nrcConstraints s = sortBy length3rd 
    [(r,c, freeAtPosNrc s (r,c)) | 
                       (r,c) <- openPositions s ]
					   
extendNrcNode :: Node -> Constraint -> [Node]
extendNrcNode (s,nrcConstraints) (r,c,vs) = 
   [(extend s (r,c,v),
     sortBy length3rd $ 
         prune (r,c,v) nrcConstraints) | v <- vs ]

initNrcNode :: Grid -> [Node]
initNrcNode gr = let s = grid2sud gr in 
              if (not . nrcConsistent) s then [] 
              else [(s, nrcConstraints s)]
			  
solveNrcNs :: [Node] -> [Node]
solveNrcNs = search succNrcNode solved 

succNrcNode :: Node -> [Node]
succNrcNode (s,[]) = []
succNrcNode (s,p:ps) = extendNrcNode (s,ps) p 

solveAndShowNrc :: Grid -> IO[()]
solveAndShowNrc gr = solveShowNrcNs (initNrcNode gr)

solveShowNrcNs :: [Node] -> IO[()]
solveShowNrcNs ns = sequence $ fmap showNode (solveNrcNs ns)
		
nrcExample :: Grid
nrcExample = [[0,0,0,0,0,0,0,0,0],
            [0,0,0,0,0,0,0,7,0],
            [0,0,0,0,0,2,3,0,0],
			[0,0,0,6,3,0,5,0,0],
			[7,5,0,0,4,0,8,0,0],
			[0,0,0,0,0,0,0,4,0],
			[0,0,0,5,0,0,0,0,0],
		    [9,1,0,7,0,0,0,6,0],
		    [0,0,7,0,0,0,0,0,0]]

{-nrcExample = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,0,9,1,6,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]-}









































