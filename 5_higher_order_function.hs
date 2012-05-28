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
-- examples
zipWith' (+) [4,2,5,6] [2,6,2,3]
zipWith' max [6,3,2,1] [7,3,1,5]
zipWith' (++) ["foo", "bar", "baz"] ["fighters", "hoppers", "aldrin"]
zipWith' (*) (replicate 5 2) [1..]
zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

zip [1,2,3,4,5] "hello"
flip' zip [1,2,3,4,5] "hello"
zipWith' div [2,2..] [10,8,6,4,2]
zipWith' (flip' div) [2,2..] [10,8,6,4,2]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) f x : map' f xs
-- examples
map (+3) [1,5,3,1,6]
map (++ "!") ["BIFF", "BANG", "POW"]
map (replicate 3) [3..6]
map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
map fst [(1,3),(3,5),(6,3),(2,6),(2,5)]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs
-- examples
filter' (>3) [1,5,3,2,1,6,4,3,2,1]
filter' (==3) [1,2,3,4,5]
filter' even [1..10]
let notNull x = not (null x) in filter' notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
filter' (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
filter' (`elem` ['A'..'Z']) "i LAuGh at you bEcause u R all the same"

filter' (<15) (filter' even [1..20])
-- equivalent to -- I prefer comprehension here.
[x | x <- [1..20], x < 15, even x]

quicksort' :: Ord a => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let smallerOrEqual = filter' (<= x) xs
	larger = filter' (> x) xs
    in  quicksort' smallerOrEqual ++ [x] ++ quicksort' larger

largestDivisible :: Integer
largestDivisible = head (filter' p [100000,99999..])
    where p x = x `mod` 3829 == 0

takeWhile (/=' ') "elephants know how to party"

sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n = n : chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- lambdas
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- equivalent
map (+3) [1,6,3,2]          -- better
map (\x -> x + 3) [1,6,3,2]

zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z               -- more readable
-- is equivalent to
addThree' :: Int -> Int -> Int -> Int
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

zipWith (flip (++) ["love you", "love me"] ["i ", "you "]
map (flip subtract 20) [1,2,3,4]


-- folding

sum' :: Num a => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' [3,5,2,1]
-- or
sum'' :: Num a => [a] -> a
sum'' = foldl (+) 0


-- defining map with foldr
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- or with foldl
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- ++ is slower than :
-- so foldr is usually used to build new lists from a list

elem' :: Eq a => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum' :: Ord a => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
reverse'' = foldl (flip (:)) []

product' :: Num a => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- foldr => f acc [a,b,c,d]
--       => f a (f b (f c (f d z)))
-- foldl => f acc [a,b,c,d]
--       => f (f (f (f acc a) b) c) d

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs
-- and' [True, False, True] => True && (False && (True && True))
