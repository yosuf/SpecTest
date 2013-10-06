module Assignment3

where

import Data.List
import Week5

nrc :: Grid
nrc =  [[0,0,0,3,0,0,0,0,0],
        [0,0,0,7,0,0,3,0,0],
        [2,0,0,0,0,0,0,0,8],
        [0,0,6,0,0,5,0,0,0],
        [0,9,1,6,0,0,0,0,0],
        [3,0,0,0,7,1,2,0,0],
        [0,0,0,0,0,0,0,3,1],
        [0,8,0,0,4,0,0,0,0],
        [0,0,2,0,0,0,0,0,0]]

{-
See Week5.hs for the adapted solver for NRC style sudoku's

Adaptions carried out:
 line 213: added nrcblocks
 line 264: added nrcbl
 line 271: added nrcGrid
 line 288: added freeInNrcGrid
 line 296: added intersection with freeInNrcGrid to freeAtPos
 line 357: added includes test for NRC-subblocks to sameblock
 - 

Solution of the sudoku above:

+-------+-------+-------+
| 4 7 8 | 3 9 2 | 6 1 5 |
| 6 1 9 | 7 5 8 | 3 2 4 |
| 2 3 5 | 4 1 6 | 9 7 8 |
+-------+-------+-------+
| 7 2 6 | 8 3 5 | 1 4 9 |
| 8 9 1 | 6 2 4 | 7 5 3 |
| 3 5 4 | 9 7 1 | 2 8 6 |
+-------+-------+-------+
| 5 6 7 | 2 8 9 | 4 3 1 |
| 9 8 3 | 1 4 7 | 5 6 2 |
| 1 4 2 | 5 6 3 | 8 9 7 |
+-------+-------+-------+

-}