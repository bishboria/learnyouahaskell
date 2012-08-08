-- A FISTFUL OF MONADS

-- In this chapter we'll learn that monads are just beefed-up applicative
-- functors. Much like applicative functors are beefed-up functors.


-- Upgrading our Applicative Functors
--
-- With Functors, we saw it was possible to map functions over various data types using Functor type class.
fmap :: (Functor f) => (a -> b) -> f a -> f b
--
-- What if the function a -> b is already wrapped inside a functor value?
-- like Just (*3) applied to Just 5... Enter Applicative type class:
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
pure  :: (Applicative f) => a -> f a

(*) <$> Just 2 <*> Just 8
-- Just 16
(++) <$> Just "klingon" <*> Nothing
-- Nothing
(-) <$> [3,4] <*> [1,2,3]
-- [2,1,0,3,2,1]
--
-- Not we can treat them as applicative values, Maybe a values represent
-- computations that might have failed, [a] values represent computations
-- that have several results (nondeterministic computations), IO a values
-- represent values that have side effects, etc.

-- Monads are a natural extension of applicative functors and the provide
-- a solution to the following problem: If we have a value with a context,
-- m a, how do we apply to it a function that takes a normal a and returns
-- a value with a context?
--
-- I.e. how do we apply a -> m b to a value of type m a?
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- >>= is called bind.


-- Getting Your Feet Wet With Maybe

-- It should be of no surprise that Maybe is a monad.

-- When we looked at Maybe as a functor:
fmap (++"!") (Just "wisdom")
-- Just "wisdom!"
fmap (++"!") Nothing
-- Nothing

-- Maybe as an applicative functor:
Just (+3) <*> Just 3
-- Just 6
Nothing <*> Just "greed"
-- Nothing
Just ord <*> Nothing
-- Nothing
--
-- using applicative style:
max <$> Just 3 <*> Just 6
-- Just 6
max <$> Just 3 <*> Nothing
-- Nothing

-- How would we use >>= with Maybe? >>= takes a monadic value and a function
-- that takes a normal value and returns a monadic value. The returned
-- monadic value has had the function applied to the original monadic value.
--
-- Here is a function from a -> Maybe b
(\x -> Just (x+1)) 1
-- Just 2
(\x -> Just (x+1)) 100
-- Just 101

-- For now, let's call >>= "applyMaybe":
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

Just 3 `applyMaybe` \x -> Just (x+1)
-- Just 4
Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
-- Just "smile :)"
Nothing `applyMaybe` \x -> Just (x+1)
-- Nothing
Nothing `applyMaybe` \x -> Just (x ++ " :)")
-- Nothing

-- What if the function returns Nothing?
Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing
-- Just 3
Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing
-- Nothing
--
-- If the value on the left is Nothing, the result is Nothing.
-- If the function on the right returns Nothing, the result is Nothing.

-- How is this useful? When we used applicative functors we could at least
-- apply ordinary functions to applicative values. We'll see that since
-- monads are an upgraded version of applicative functors we can also do
-- that. Will also see other things that applicative functors can't do.
