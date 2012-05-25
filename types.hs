-- can't load this file
'a' :: Char --  :: read as "has type of"
True :: Bool
"HELLO!" :: [Char]
(True, 'a') :: (Bool, Char)
4 == 5 :: Bool

-- function definiton with type declaration
removeNonUpperCase :: [Char] -> [Char] -- Takes one parameter, a [Char]
				       -- Returns [Char]
removeNonUpperCase st = [c | c  <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int -- Takes 3 Ints, Returns Int
addThree x y z = x + y + z


-- type variables. Quite like generics. But more powerful
-- functions that use type variables are called
--	 polymorphic functions
:t head
head :: [a] -> a -- a can be any type

:t fst
fst :: (a, b) -> a -- a, b could be any type.


-- type classes.
:t (==)
(==) :: (Eq a) => a -> a -> Bool

:t (>)
(>) :: (Ord a) => a -> a -> Bool

"Abrakadabra" < "Zebra"
("Abrakadabra" `compare` "Zebra") == LT

show 3
show 5.334
show True

read "True" || False
read "8.2" + 3.8
read "5" - 2
read "[1,2,3,4]" ++ [3]
read "4" -- Ambiguous type variable 'a'.

:t read
read :: Read a => String -> a
