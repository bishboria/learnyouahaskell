import Data.Monoid
import Control.Monad.Writer

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- Here is the monoid instance:
instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

-- Notice how mempty is just id. And mappend is just function composition.

result = fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
-- [1,2,3,4,1,2,3]

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

gcdresult = mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse' 100 34
-- Finished with 2
-- 32 mod 2 = 0
-- 34 mod 32 = 2
-- 100 mod 34 = 32

finalCountDownFast :: Int -> Writer (DiffList String) ()
finalCountDownFast 0 = do
    tell (toDiffList ["0"])
finalCountDownFast x = do
    finalCountDownFast (x-1)
    tell (toDiffList [show x])

finalCountDownSlow :: Int -> Writer [String] ()
finalCountDownSlow 0 = do
    tell ["0"]
finalCountDownSlow x = do
    finalCountDownSlow (x-1)
    tell [show x]

countdownFast = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDownFast 500000
countdownSlow = mapM_ putStrLn . snd . runWriter $ finalCountDownSlow 500000
