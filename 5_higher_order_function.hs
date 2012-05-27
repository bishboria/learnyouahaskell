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
