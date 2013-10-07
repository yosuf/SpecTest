module Assignment4 where

import RandomSudoku

{-
    changed lines in RandomSudoku.hs

    - line 10: import Week5 -> Week5Adapted

    This change is enough to adapt the RandomSudoku to generate NRC-style sudoku's
    because the generator is defined in terms of the solver's constraints.

    Note: it takes significantly more time to generate NRC sudoku's than standard sudoku's.
-}