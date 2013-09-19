

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

{- 2.11
1. law of doubel negation 
     p ≡  ¬  ¬  P          {- ASCII for not : ALT + 170 -}
     T    T  F  T     
 	 F	  F  T  F   

2. laws of idempotence 
	P  ^  P  ≡  P  ;  p  V   p  ≡ p 
	T  T  T     T     T  T   T    T 
	F  F  F     F     F  F   F    F

3. (P  => Q) ≡ ¬  P   v  Q    ;   ¬ ( p  => Q) ≡  p  ^  ¬ Q
    T  T   T    F  T  T  T        F   T  T   T    T  F  F  T   
    T  F   F    F  T  F  F        T   T  F   F    T  T  T  F
    F  T   T    T  F  T  T        F   F  T   T    F  F  F  T 
    F  T   F    T  F  T  F        F   F  T   F    F  F  T  F
See the value of column => and v. you need to consider operator precedene



4. Laws of contraposition 
	(¬ P => ¬ Q ) ≡ (Q => P) ;   (p => ¬ Q) ≡ ( Q => ¬ P) ; ( ¬ P => Q) ≡ ( ¬ Q => P)


5.  ( P <=> Q) ≡ ((p => Q) ^ ( Q => P)) ≡ (( p ^ Q) v ( ¬ P ^ ¬ Q ))


6. Laws of commutativity 	
    P ^ Q ≡  Q ^ P   ;   P v Q ≡ Q v P


7. DeMorgan laws
	¬( P ^ Q) ≡  ¬ P v ¬ Q  ;   ¬( P v Q )  ≡ ¬ P ^ ¬ Q

8. Laws of associativity 
   P ^ ( Q ^ R )≡ (P ^ Q ) ^ R     P v ( Q v R) ≡ ( P v Q) v R

9. Distribution laws
   P  ^ ( Q  v   R ) = ( P  ^  Q ) v ( P  ^  R)  ;   P  v  ( Q  ^  R ) =  ( P  v  Q ) ^  ( P  v   R
   T  T   T  T   T       T  T   T  T   T  T  T       T  T    T  T  T        T  T  T   T    T  T   T
   T  T   T  T   F       T  T   T  T   T  F  F       T  T    T  F  F        T  T  T   T    T  T   F
   T  T   F  T   T       T  F   F  T   T  T  T       T  T    F  F  T        T  T  F   T    T  T   T
   T  F   F  F   F       T  F   F  F   T  F  F       T  T    F  F  F        T  T  F   T    T  T   F
   F  F   T  T   T       F  F   T  F   F  F  T       F  T    T  T  T        F  T  T   T    F  T   T
   F  F   T  T   F       F  F   T  F   F  F  F       F  F    T  F  F        F  T  T   F    F  F   F
   F  F   F  T   T       F  F   F  F   F  F  T       F  F    F  F  T        F  F  F   F    F  T   T
   F  F   F  F   F       F  F   F  F   F  F  F       F  T    F  F  F        F  F  F   F    F  F   F
      *                            *                    *                             *      

-} 

{- 2.13        ┴ = Alt + 193  ┬  = Alt + 194  
  ¬ ┬  ≡ ┴      ;   ¬ ┴ ≡ ┬      -}







