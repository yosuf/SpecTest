
double :: Int -> Int
double n = 2 * n



sentence = "Sentences can go " ++ onAndOn
onAndOn = "on and " ++ onAndOn


sentenceMap = "Sentence can go " :
                               map (++ " and on ") sentenceMap





{- Lambda Abstraction In Haskel -}
sqr :: Int -> Int
sqr = \x -> x * x


{- Lambda abstractions and concatenation -}
name = ( \x      ->    x ++ "Shrestha" ) "Prajan "


reversal :: [a] -> [a]   {- [a] is  list over a -}
{-reversal [] = [] -}  
reversal (x : t) = reversal  t  ++   [x]      {- the head is x and tail is t for reversal -}




{- | 1.1 precedene : ( ), / , * , + , -  [ BODMAS ]     -}

