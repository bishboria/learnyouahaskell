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
