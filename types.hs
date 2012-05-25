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

-- Eq
:t (==)
(==) :: (Eq a) => a -> a -> Bool

-- Ord
:t (>)
(>) :: (Ord a) => a -> a -> Bool
"Abrakadabra" < "Zebra"
("Abrakadabra" `compare` "Zebra") == LT

-- Show
:t show
show :: Show a => a -> String

show 3
show 5.334
show True

-- Read
:t read
read :: Read a => String -> a

read "True" || False
read "8.2" + 3.8
read "5" - 2
read "[1,2,3,4]" ++ [3]
read "4" -- Ambiguous type variable 'a'.
read "5" :: Int
read "5" :: Float
(read "5" :: Float) * 4
read "[1,2,3,4]" :: [Int]
read "(3, 'a')" :: (Int, Char)
[read "True", False, True, False]

:t [1..3]
[1..3] :: (Num t, Enum t) => [t]
['a'..'e']
[LT..GT]
