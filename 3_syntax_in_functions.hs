-- Pattern Matching

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- with tuples

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x       -- _ ignored as seen in list comprehensions

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z       -- binding to several variables (including _)
                          -- need to wrap in parens

-- with lists and comprehensions
xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
summed = [a+b | (a,b) <- xs]


head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x


tell :: Show a => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. First two elements are: " ++ show x ++ " and " ++ show y

-- (x:[]) and (x:y:[]) could be rewritten as [x] and [x,y]
-- (x:y:_) can't be rewritten like that though.

badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z


-- As-patterns
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- Guards
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"    	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"

max' :: Ord a => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

compare' :: Ord a => a -> a -> Ordering
a `compare'` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"    	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pfft, I bet you're ugly!"    	| bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
	  skinny = 18.5
	  normal = 25.0
	  fat = 30.0

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan"
greet "Fernando" = niceGreeting ++ " Fernando"
greet name = badGreeting ++ " " ++ name
    where niceGreeting = "Hello! So very nice to see you,"
	  badGreeting = "Oh! Pfft. It's you."
