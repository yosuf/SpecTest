

module Sol2 where import TAMO









{- 2.2 : truth table for exlclusive version of OR

P 	Q 	p XOR Q
t 	t 	f
t 	f 	t
f 	t 	t 
f 	f 	f 


-}

{- 2.9

(p xor  Q) xor   Q    <=>   P

 t  f   t   t    t     t    t
 t  t   f   t    f     t    t 
 f  t   t   f    t     t    f
 f  f   f   f    f     t    f

The value of Equivalence (<=>) with all set to True (t) shows that it is equivalent.

-}