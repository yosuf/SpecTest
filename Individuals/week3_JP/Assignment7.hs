module Assignment7 where

import Week3
import Assignment3

--type Name = String
getRandomName :: IO Name
getRandomName = do n <- getRandomInt 2
                   return (["x", "y", "z"] !! n)          
                   
--data Term = V Name | F Name [Term] deriving (Eq,Ord)  
getRandomTerm :: Int -> IO Term                 
getRandomTerm l = do  x <- getRandomInt 1
                      case x of
                        0 -> do n <- getRandomName
                                return (V n)
                        1 -> do n <- getRandomName
                                t <- getRandomTerms l
                                return (F n t)

getRndTerms :: Int -> IO [Term]
getRndTerms 0 = return []
getRndTerms n = do  t <- getRandomTerm n
                    ts <- getRndTerms (n-1)
                    return (t:ts)
                      
getRandomTerms :: Int -> IO [Term]       
getRandomTerms 0 = return []            
getRandomTerms l = do x <- getRandomInt l
                      getRndTerms x
                      
getRandomAtom :: Int -> IO Formula                     
getRandomAtom l = do  t <- getRandomTerms l
                      return (Atom "R" t)

getRndForms :: Int -> Int -> IO [Formula]
getRndForms _ 0 = return []
getRndForms l n = do  f <- getRandomFormula l
                      fs <- getRndForms l (n-1)
                      return (f:fs)
                        
getRandomFormulas :: Int -> IO [Formula]
getRandomFormulas 0 = return []
getRandomFormulas l = do  x <- getRandomInt l
                          f <- getRndForms l x
                          return f
                        
getRandomFormula :: Int -> IO Formula
getRandomFormula 0 = getRandomAtom 2
getRandomFormula l = do x <- getRandomInt 8
                        case x of
                          0 -> getRandomAtom l
                          1 -> do t1 <- getRandomTerm l
                                  t2 <- getRandomTerm l
                                  return (Eq t1 t2)
                          2 -> do f <- getRandomFormula (l-1)
                                  return (Neg f)
                          3 -> do f1 <- getRandomFormula (l-1)
                                  f2 <- getRandomFormula (l-1)
                                  return (Impl f1 f2)
                          4 -> do f1 <- getRandomFormula (l-1)
                                  f2 <- getRandomFormula (l-1)
                                  return (Equi f1 f2)
                          5 -> do f <- getRandomFormulas (l-1)
                                  return (Conj f)
                          6 -> do f <- getRandomFormulas (l-1)
                                  return (Disj f)
                          7 -> do n <- getRandomName
                                  f <- getRandomFormula (l-1)
                                  return (Forall n f)
                          8 -> do n <- getRandomName
                                  f <- getRandomFormula (l-1)
                                  return (Exists n f)

getRndFormula = do  x <- getRandomInt 5
                    (getRandomFormula (x+1))
                                