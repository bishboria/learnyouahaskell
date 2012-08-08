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
