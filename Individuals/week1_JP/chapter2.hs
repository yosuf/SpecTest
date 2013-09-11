module Chapter2 where
import TAMO

{- 2.2
  P  Q  | P xor Q
------------------
  t  t  |   f
  t  f  |   t
  f  t  |   t
  f  f  |   f
-}

-- 2.13
test2_12_1a = lequiv (not True) False
test2_12_1b = lequiv (not False) True
test2_12_2 = lequiv (\ p -> p ==> False) (\ p -> not p )
test2_12_3a = lequiv (\ p -> p || True) (\ p -> True)
test2_12_3b = lequiv (\ p -> p && False) (\ p -> False)
test2_12_4a = lequiv (\ p -> p || False) (\ p -> p)
test2_12_4b = lequiv (\ p -> p && True) (\ p -> p)
test2_12_5 = lequiv (\ p -> p || (not p)) (\ p -> True)
test2_12_6 = lequiv (\ p -> p && (not p)) (\ p -> False)

-- run to check all equivalences
checkall2_12 = and [ p | p <- [test2_12_1a, test2_12_1b, test2_12_2, test2_12_3a, test2_12_3b, test2_12_4a, test2_12_4b, test2_12_5, test2_12_6] ]

-- 2.15
contra1 b = b && (not b)
contra2 b1 b2 = (b1 || b2) <+> (b1 || b2)

is_contradiction1 :: (Bool -> Bool) -> Bool
is_contradiction1 f = and [not (f b) | b <- [True, False]]
is_contradiction2 :: (Bool -> Bool -> Bool) -> Bool
is_contradiction2 f = and [not (f b1 b2) | b1 <- [True, False],
                                          b2 <- [True, False]]
is_contradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
is_contradiction3 f = and [not (f b1 b2 b3) | b1 <- [True, False],
                                              b2 <- [True, False],
                                              b3 <- [True, False]]

-- 2.16