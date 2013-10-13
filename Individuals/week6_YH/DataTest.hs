module DataTest where

data Leaf = String

data Btree a = Leaf a | Node (Btree a) (Btree a) deriving (Eq,Show)




data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)  




data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show) 