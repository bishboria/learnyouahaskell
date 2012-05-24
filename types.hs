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
