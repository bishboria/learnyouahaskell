import Control.Monad.Instances

-- Reminder
-- instance Functor (Either a) where
--    fmap f (Right x) = Right (f x)
--    fmap f (Left  x) = Left  x

a = fmap (replicate 3) [1,2,3,4]
b = fmap (replicate 3) (Just 4)
c = fmap (replicate 3) (Right "blah")
d = fmap (replicate 3) Nothing
e = fmap (replicate 3) (Left "foo")
