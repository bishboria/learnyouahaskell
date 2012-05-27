-- Can't run this file

-- currying

max 4 5
(max 4) 5

:t max
max :: Ord a => a -> a -> a
-- could be written as
max :: Ord a => a -> (a -> a)

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

multTwoWithNine = multThree 9
multTwoWithNine 2 3

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x
-- which is equivalent to
compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

-- Sections
divideByTen :: Floating a => a ->> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- higher orderism
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- examples
applyTwice (+3) 10
applyTwice (++ " HAHA") "HEY"
applyTwice ("HAHA " ++) "HEY"
applyTwice (3:) [1]

-- implementing zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
