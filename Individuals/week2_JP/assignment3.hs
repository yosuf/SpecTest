module Assignment3

where

import Week2

{-
  Conjunctive Normal Form

  L ::= p | neg p
  D ::= L | L or D
  C ::= D | D and C
-}

-- precondition: input is in negation normal form
cnf :: Form -> Form
cnf (Dsj [p, Cnj [q, r]]) = Cnj[cnf(Dsj [p,q]), cnf(Dsj [p,r])]
cnf (Dsj [Cnj [p,q], r]) = Cnj[cnf(Dsj [p,r]), cnf(Dsj [q,r])]
cnf (Dsj fs) = Dsj (map cnf fs)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf f = f
-- postcondition: output is in conjunctive normal form

{-
  pre  : propositional formula
  post : True when no Cnj's are inside Dsj's
-}
test (Cnj f) = and (map test f)
test (Dsj f) = and (map testd f)
test f = True

testd (Dsj f) = and (map testd f)
testd (Cnj f) = False
testd f = True


{-
  
  TimeSpend = 120 minutes
  
-}