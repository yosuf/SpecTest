module Assignment6 where

import Week2Module
import Assignment3

-- rename getRandomInt'' from Assignment3
getRandomInt = getRandomInt''

getRandomF :: IO Form
getRandomF = do d <- getRandomInt 4
                getRandomForm d
                
getRandomForm :: Int -> IO Form
getRandomForm 0 = do  m <- getRandomInt 20
                      return (Prop (m+1))
                      
getRandomForm d = do  n <- getRandomInt 3
                      case n of
                        0 -> do m <- getRandomInt 20
                                return (Prop (m+1))
                        1 -> do f <- getRandomForm (d-1)
                                return (Neg f)
                        2 -> do m <- getRandomInt 5
                                fs <- getRandomForms (d-1) m
                                return (Cnj fs)
                        3 -> do m <- getRandomInt 5
                                fs <- getRandomForms (d-1) m
                                return (Dsj fs)
                                
getRandomFs :: Int -> IO [Form]
getRandomFs n = do  d <- getRandomInt 3
                    getRandomForms d n
                    
getRandomForms :: Int -> Int -> IO [Form]
getRandomForms _ 0 = return []
getRandomForms d n = do
                        f <- getRandomForm d
                        fs <- getRandomForms d (n-1)
                        return (f:fs)
                        
                        
test :: Int -> (Form -> Bool) -> [Form] -> IO ()
test n _ [] = print (show n ++ " tests passed")
test n p (f:fs) =
  if p f
  then do print ("pass on:" ++ show f)
          test n p fs
  else error ("failed test on:" ++ show f)
  
testForms :: Int -> (Form -> Bool) -> IO ()
testForms n prop = do
                      fs <- getRandomFs n
                      test n prop fs


-- Code from last week -->
cnf :: Form -> Form
cnf (Dsj [p, Cnj [q, r]]) = Cnj[cnf(Dsj [p,q]), cnf(Dsj [p,r])]
cnf (Dsj [Cnj [p,q], r]) = Cnj[cnf(Dsj [p,r]), cnf(Dsj [q,r])]
cnf (Dsj fs) = Dsj (map cnf fs)
cnf (Cnj fs) = Cnj (map cnf fs)
cnf f = f

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = contradiction (Neg f)

equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

{-
  pre  : propositional formula
  post : True when no Cnj's are inside Dsj's
-}
isCnf (Cnj f) = and (map isCnf f)
isCnf (Dsj f) = and (map isCnf' f)
isCnf f = True

isCnf' (Dsj f) = and (map isCnf' f)
isCnf' (Cnj f) = False
isCnf' f = True
-- <-- code from last week

testCnf :: IO ()
testCnf = testForms 1000 (\f -> let f' = (cnf (nnf (arrowfree f))) in (equiv f f') && (isCnf f'))

{--
  Test Report
  
  testCnf tests if the cnf function creates a proper CNF representation and if that
  CNF representation of a truth formula f is equivalent to that formula f.
  testCnf runs thousand times with random generated formula's.
  
  From last week's feedback we know that the cnf function is incomplete.
  However, testCnf needs to run several times before it discovers non-CNF functions.
  
--}




