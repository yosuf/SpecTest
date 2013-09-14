
addThree :: Int -> Int -> Int -> Int
addThree x y z = x+y+z

doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y 

doubleSmallNumber x =	if x < 100
			then doubleMe x
			else x

doubleSmallNumber' x = 1+ (if x < 10 then doubleMe x else x)

yosufHaydary = "It's me, Yosuf Haydary"

boomBangs xs = [ if odd x then "BOOM!" else "BANG!" | x <- xs, x `mod` 3 == 1 ]


filtering xs = [ x | x <- xs, odd x, even x]

-- this function returns the list of x*y where x<5 and y<10
twoVars xs ys = [x*y | x <- xs, x<5, y <- ys, y < 10]

-- our own version of length! :)
-- _ means that we don't care. it can be anything drawn from list
-- so basically it means we put 1 of anything that is drawn. Simple ;) Remember: Stirngs are also lists (of chars).
length' input = sum [1| _ <- input]


-- speaks for itself 
removeEvens input =  [ x | x <- input, not(even x) ]


-- nested lists be like =>  [[]]
nestedList input = [ [x| x<-xs] | xs <- input ] 

--flattenOut input =
-- if length input <= 0
-- then []
-- else
--  if length input == 1 then head input
--  else head input ++ (flattenOut tail input)

-- Pattern Matching --

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven! :)))))"
lucky x = "You did not win :( Better luck next time"


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * (factorial (x - 1))

charName :: Char -> String
charName 'a' = "Ahmad"
charName 'b' = "Boris"


-- Gurads

bmiText :: Float -> String
bmiText bmi
	| bmi <= 18.5 = show bmi ++ " is considered underweight"
	| bmi <= 25.0 = show bmi ++ " is considered normal"
	| bmi <= 30.0 = show bmi ++ " is considered overweight"
	| otherwise   = show bmi ++ " is considered obese"


getBmi :: Float -> Float -> String
getBmi hight weight = bmiText (weight / hight^2)



--where test
aboveFive :: Int -> Bool
aboveFive input
		| result == True = True
		| result == False = False
		where result = input > 5


doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:input) =  x*2:(iterateList input)




