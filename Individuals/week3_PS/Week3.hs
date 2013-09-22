module Week3

where 

import Data.List

type Name = String
data Term = V Name | F Name [Term] deriving (Eq,Ord)

instance Show Term where 
  show (V name)    = name
  show (F name []) = name 
  show (F name ts) = name ++ show ts

x, y, z, t :: Term    {- ps: Introduced t, t is kept in a value V-}
x = V "x"
y = V "y"
z = V "z"
t = V "t"

varsInTerm :: Term -> [Name]
varsInTerm (V name) = [name]
varsInTerm (F _ ts) = varsInTerms ts where 
  varsInTerms :: [Term] -> [Name]
  varsInTerms = nub . concat . map varsInTerm

subst :: Name -> Term -> Term -> Term
subst name t (V name') =                       {- subst "shrestha" (V "Prajan") (V "Shrestha")  : Result is Shrestha-}       
  if name == name' then t else (V name') 
subst name t (F name' ts) =                    {-ps: this is running for  subst "x" (F "f" [x,x,x,x]) (F "f" [x,x,x,x]) -}
      F name' (map (subst name t) ts) 

{- subst "x"   (F "w" [y])   (F "w" [x])  : Result is w[w[y]]  -}

{-
 name = "x"
 t = (F "w" [y]) 

 map ( subst "x" (F "w" [y])) [x,x]   : Result [w[y],w[y]]
-}










data Formula = Atom Name [Term]
               | Eq Term Term
               | Neg  Formula 
               | Impl Formula Formula
               | Equi Formula Formula
               | Conj [Formula]
               | Disj [Formula] 
               | Forall Name Formula
               | Exists Name Formula
               deriving (Eq,Ord)

instance Show Formula where 
  show (Atom s [])   = s                          {- ps: Atom "Praj" []   : Result is    Praj-}
  show (Atom s xs)   = s ++ show xs               {- ps: Atom "Praj" [x,y,t]  : Result is   Praj[x,y,t] . We canot use [x,y,d] becuase d is not assigned in V -}
  show (Eq t1 t2)    = show t1 ++ "==" ++ show t2 {- ps: Eq x y : Result is x == y -} 
  show (Neg form)    = '~' : (show form)          {- ps: Neg (Eq x y) : Result is ~x==y ; OR  Neg( Atom "Praj" []) : Result is ~Praj   -}
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>"    {- ps: Impl (Eq x y) Eq( y z) : Result is (x==y=>y==z) ;OR   Impl (Neg (Eq x y)) (Eq y z)  : Result is ( ~x==y=>y==z)-}
                           ++ show f2 ++ ")"      
  show (Equi f1 f2)  = "(" ++ show f1 ++ "<=>"    {- ps: Equi (Neg(Eq x y)) (Eq y z)  : Result is (~x==y<=>y==z )-} 
                           ++ show f2 ++ ")"
  show (Conj [])     = "true" 
  show (Conj fs)     = "conj" ++ show fs          {- ps: Conj[Neg(Eq x y)] : Result is  conj[~x==y]  -}
  show (Disj [])     = "false" 
  show (Disj fs)     = "disj" ++ show fs          {- ps: Disj[Eq x y] : Result is   disj[x==y]-}
  show (Forall v f)  = "A " ++  v ++ (' ' : show f)  {- ps: Forall "x" (Eq x z)  : Result is  A x x==z -}
  show (Exists v f)  = "E " ++  v ++ (' ' : show f)  {- ps: Exists "x" (Eq x z)  : Result is E x x==z -}

r = Atom "R" 

formula1 = Forall "x" (r [x,x])
formula2 = Forall "x" 
            (Forall "y"
              (Impl (r [x,y]) (r [y,x])))   {- ps: Result is    A x A y (R[x,y] => R(y,x) )) -}








type Rint a = Name -> [a] -> Bool   {- R stands for Relationship as characteristic funcitons for lists -}
type Fint a = Name -> [a] -> a      {- F stands for Functions as maps lists to objects-}

type Lookup a = Name -> a

termVal :: Lookup a -> Fint a -> Term -> a 
termVal g i (V name) = g name
termVal g i (F name ts) = i name (map (termVal g i) ts) 



changeLookup :: Lookup a -> Name -> a -> Lookup a 
changeLookup g v d = \ v' -> 
             if v == v' then d else g v'




evalFOL :: Eq a => [a] -> Lookup a -> Fint a -> Rint a -> Formula -> Bool
evalFOL domain g f i = evalFOL' g where 
    evalFOL' g (Atom name ts) = i name (map (termVal g f) ts)
    evalFOL' g (Eq t1 t2) = termVal g f t1 == termVal g f t2
    evalFOL' g (Neg form) = not (evalFOL' g form)
    evalFOL' g (Impl f1 f2) = not (evalFOL' g f1 && not (evalFOL' g f2))
    evalFOL' g (Equi f1 f2) = evalFOL' g f1 == evalFOL' g f2 
    evalFOL' g (Conj fs)    = and (map (evalFOL' g) fs)
    evalFOL' g (Disj fs)    = or  (map (evalFOL' g) fs)
    evalFOL' g (Forall v form) = all (\ d -> evalFOL' (changeLookup g v d) form) domain 
    evalFOL' g (Exists v form) = any (\ d -> evalFOL' (changeLookup g v d) form) domain 

f :: Fint Int
f "z" []    = 0 
f "s" [i]   = succ i   {- ps: f "s" [2] ; result is 3   -}
f "p" [i,j] = i + j     {- ps: f "p" [3,4] ; result is 7 -}
f "t" [i,j] = i * j

i :: Rint Int 
i "R" [i,j] = i < j       {- ps: i "R" [3,4] ; result is True-}

zero = F "z" []     {- ps: zero ; result is z -}

frm1 = Exists "x" (r [zero,x])          {-ps: frm1  ; Result is E x R[z,x]    -}
frm2 = Exists "x" (Exists "y" (r [x,y]))
frm3 = Forall "x" (Exists "y" (r [x,y]))

evalFOL1 = evalFOL [0..] (\ v -> 0) f i frm1        {- ps: evalFOL1 ; Result is True -}
evalFOL2 = evalFOL [0..] (\ v -> 0) f i frm2
evalFOL3 = evalFOL [0..] (\ v -> 0) f i frm3
evalFOL4 = evalFOL [0..] (\ v -> 0) f i (Neg frm3)
