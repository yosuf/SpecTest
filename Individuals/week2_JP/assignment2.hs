module Assignment2

where 

import Week2

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = contradiction (Neg f)

-- http://www.millersville.edu/~bikenaga/math-proof/truth-tables/truth-tables.html
taut01 = Dsj [p, Neg p]
taut02 = Neg (Cnj [p, Neg p])
taut03 = Neg (Neg (Equiv p p))
taut04 = Impl (Cnj [Impl p q, Neg q]) (Neg p)
taut05 = Impl p (Dsj [p,q])
tautCheck = and [tautology f | f <- [taut01, taut02, taut03, taut04, taut05]]

-- logical entailment
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

{-
  Test Report
  
  In order to test the functions, white-box testing was performed. This means that we look into
  the logic of the implementation and try to reason about the correctness.
  
  The equivalence of two functions f1 and f2 can be expressed in the language of the proposition logic itself
  If this new proposition formula holds for all valuations f1 and f2 are considered to be equivalent.
  In other words, f1 and f2 are equivalant if (Equiv f1 f2) is a tautology. As such, it is implemented.
  
  Same reasoning can be applied to entails. Entailment (or implication) can also be expressed in the language
  of the proposition logic. If the implication of (f1 f2) is a tautology, f2 logically follows from f1. As such,
  it is implemented.
  
  The function for determining tautology is build upon the funtion for contradiction. In turn, the function for
  contradiction is build upon the function for satisfiability which was given.
  
  Now we test the function for tautology by defining several well known tautologies. If the function holds
  for all defined tautologies, it is believed that the function is correct.
  
  Run tautCheck to test with the provided tautologies.
  
  TimeSpend = +/- 120 mins
  
-}