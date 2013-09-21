#! /usr/bin/runhaskell

main :: IO ()
main = do
        let x = Var "x"
        let y = Var "y"
        let z = Var "z"
        let f terms = Func "f" terms
        let g terms = Func "g" terms
        let p terms = Pred "P" terms
        let r terms = Pred "R" terms
        putStrLn (show x ++ " is a variable")
        putStrLn (show (f [x, g [y]]) ++ " is a function symbol applied to two terms")
        putStrLn ((show (Forall "y" (Not (And (Equal x y) (p [y, f [x, z]]))))) ++ " is a rather long formula")
        putStrLn ((show (subst "x" (g [z]) (Forall "y" (Not (And (Equal x y) (p [y, f [x, z]]))))) ++ " is the same formula with g(z) substituted for x"))

-- A term is a variable or a function applied to a list of terms
data Term = Var String | Func String [Term]

insertCommas :: [String] -> String
insertCommas [x] = x
insertCommas (x:xs) = x ++ ", " ++ insertCommas xs

instance Show Term where
    show (Var name) = name
    show (Func name []) = name
    show (Func name terms) = name ++ "(" ++ insertCommas (map show terms) ++ ")"

-- A formula is an assertion of equality between two terms
-- or a predicate symbol applied to a list of terms
-- or the conjunction of two formulae
-- or the disjunction of two formulae
-- or the negation of a formula
-- or a formula preceded by universal quantification over a variable
-- or a formula preceded by existential quantification over a variable

data Formula = Equal Term Term | Pred String [Term] | And Formula Formula | Or Formula Formula | Not Formula | Forall String Formula | Exists String Formula

instance Show Formula where
     show (Equal t1 t2) = show t1 ++ " = " ++ show t2
     show (Pred name []) = name
     show (Pred name terms) =  name ++ "(" ++ insertCommas (map show terms) ++ ")"
     show (And f1 f2) = "(" ++ show f1 ++ " /\\ " ++ show f2 ++ ")"
     show (Or f1 f2) = "(" ++ show f1 ++ " \\/ " ++ show f2 ++ ")"
     show (Not f) = "~" ++ show f
     show (Forall name f) = "A[" ++ name ++ "]" ++ "(" ++ show f ++ ")"
     show (Exists name f) = "E[" ++ name ++ "]" ++ "(" ++ show f ++ ")"


subsTerm :: String -> Term -> Term -> Term
subsTerm variable newterm (Var var)
    | variable == var    = newterm
    | otherwise          = Var var
subsTerm variable newterm (Func f terms) = Func f (map (subsTerm variable newterm) terms)

-- We'll use F[x/t] to denote the formula F with all free occurences
-- of x replaced by t

subst :: String -> Term -> Formula -> Formula

-- (t1 = t2)[x/t] === (t1[x/t] = t2[x/t])

subst variable newterm (Equal t1 t2) = Equal (subsTerm variable newterm t1) (subsTerm variable newterm t2)

-- P(t1, ..., tn)[x/t] === P(t1[x/t], ..., tn[x/t])

subst variable newterm (Pred p terms) = Pred p (map (subsTerm variable newterm) terms)

-- (R /\ S)[x/t] === (R[x/t] /\ S[x/t])

subst variable newterm (And f1 f2) = And (subst variable newterm f1) (subst variable newterm f2)


-- (R \/ S)[x/t] === (R[x/t] \/ S[x/t])

subst variable newterm (Or f1 f2) = Or (subst variable newterm f1) (subst variable newterm f2)

-- (¬R)[x/t] === ¬(R[x/t])

subst variable newterm (Not f) = Not (subst variable newterm f)

-- A[y](R)[x/t] ===
--  if x == y then A[y](R)
--  else           A[y](R[x/t])

subst variable newterm (Forall var f)
    | variable == var    = Forall var f
    | otherwise          = Forall var (subst variable newterm f)

-- E[y](R)[x/t] ===
--  if x == y then E[y](R)
--  else           E[y](R[x/t])

subst variable newterm (Exists var f)
    | variable == var    = Exists var f
    | otherwise          = Exists var (subst variable newterm f)
