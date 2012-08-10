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


-- The Monad Type Class
class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>)  :: m a -> m b -> m b
    x >> y = x >>= \_ -> y

    fail    :: String -> m a
    fail msg = error msg
-- it doesn't say class (Applicative m) => Monad m where because when
-- Haskell was made it didn't occur that applicative functors were a good
-- fit for Haskell... But every monad is an applicative functor.
--
-- return is the same as pure. It takes a value and wraps it in a minimal
-- default context. I.e. It takes a value and wraps it in a monad.
--
-- >>= is like function application, but it takes a monadic value and feeds
-- it to a function that takes a normal value and returns a monadic value.
--
-- We'll talk about >> and fail later.

-- Maybe an instance of Monad
instance Monad Maybe where
    return x      = Just x
    Nothing >>= f = Nothing
    Just x  >>= f = f x
    fail _        = Nothing
-- return is exactly the same as pure.
-- >>= is just like applyMaybe
return "WHAT" :: Maybe String
-- Just "WHAT"
Just 9 >>= \x -> return (x*10)
-- Just 90
Nothing >>= \x -> return (x*10)
-- Nothing


-- Walk The Line
--
-- Pierre is taking up tightrope walking. He is quite good, until birds
-- come and rest on the pole. He can retain balance as long as the birds
-- on the left and right differ by less than 4

type Birds = Int
type Pole  = (Birds, Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

landLeft 2 (0,0)
-- (2,0)
landRight 1 (1,2)
-- (3,1)
landRight (-1) (1,2)
-- (1,1)
landLeft 2 (landRight 1 (landLeft 1 (0,0)))
-- (3,1)
-- The bottom example shows that it a bit awkward to apply chains of birds
-- landing on the pole.
-- We could use the following function:
(-:) :: a -> (a -> b) -> b
x -: f = f x
-- Now we can have the parameter on the left and the function on the right
(0,0) -: landLeft 1 -: landRight 1 -: landLeft 2
-- (3,1)

-- The above looks much cleaner now.


-- I'll Fly Away

-- It all looks fine so far, but:
landLeft 10 (0,3)
-- (10,3) Pierre would have fallen off the tightrope.
(0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
-- (0,2) landLeft (-1) Pierre would have fallen off the tightrope.
--
-- We need something to indicate a failure... Maybe to the rescue.

-- fixing the land functions:
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing
-- Now the functions return a Maybe Pole. If the number of birds on any side
-- cause Pierre to lose balance, we get Nothing indicating failure.
landLeft 2 (0,0)
-- Just (2,0)
landLeft 10 (0,3)
-- Nothing

-- Because we are now returning a Maybe Pole, we can't chain up calls as
-- before... Now, we can use >>= instead!

landRight 1 (0,0) >>= landLeft 2
-- Just (2,1)

-- Remember landLeft 2 has a type Pole -> Maybe Pole. We can't just give
-- it a Maybe Pole, so we have to get the value out of the monad first,
-- hence we use >>=

Nothing >>= landLeft 2
-- Nothing
return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
-- Just (2,4)

-- Previously we had this:
(0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
-- (0,2)
-- which is incorrect. Now we have:
return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
-- Nothing
-- Which is correct!

-- We couldn't have done this just using Maybe as an applicative.
-- Applicative functors don't allow for the applicative values to interact
-- with each other very much. They can, at best, be used as parameters to a
-- function by using the applicative style.
--
-- Here each step relies on the result of the previous. On every landing,
-- the possible result from the previous is examined and the pole is checked
-- for balance. This determines whether the landing will succeed or fail.


-- Banana on a Wire

-- Here's a function that ignores the current number of birds on the
-- balancing pole and just makes Pierre slip and fall.
banana  :: Pole -> Maybe Pole
banana _ = Nothing

return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- Nothing

-- instead of making functions that ignore their input and just return a
-- predetermined monadic value, we can use >>:
(>>)  :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n
-- Normally, passing a value to a function that ignores its parameter and
-- returns some predetermined value always results in that predetermined
-- value. However, with monads the context and meaning has to be taken into
-- account:
Nothing >> Just 3
-- Nothing
Just 3 >> Just 4
-- Just 4
Just 3 >> Nothing
-- Nothing

-- Replace >> with >>= \_ -> and it's easy to see what's going on.
return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1

-- What would this look like if we hadn't treated Maybe values as values
-- with a failure context?
routine :: Maybe Pole
routine = case landLeft 1 (0,0) of
    Nothing    -> Nothing
    Just pole1 -> case landRight 4 pole1 of
        Nothing    -> Nothing
        Just pole2 -> case landLeft 2 pole2 of
            Nothing    -> Nothing
            Just pole3 -> landLeft 1 pole3
-- We land a bird on the pole, check for the possibility for failure.
-- If it failed, return Nothing. Otherwise repeat the process.
--
-- Clearly chaining up monadic applications with >>= saves a lot of time
-- when do computations that are based on computations that might have
-- failed.
--
-- Maybe's implementation of >>= features exactly the logic checking for
-- failure and ending, or success then continuing.


-- do Notation

-- Monads in Haskell are so useful they were given their own syntax called
-- do notation. Used for gluing together several actions into one. We saw
-- this in Chapter 8 using IO actions. Consider the following:
Just 3 >>= (\x -> Just (show x ++ "!"))
-- Just "3!"
Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- Just "3!"
-- Which has the same feel as:
let x = 3; y = "!" in show x ++ y
-- The main difference is that the values are monadic:
Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- Nothing
Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))
-- Nothing
Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing)
-- Nothing
-- It's kind of like assigning values to variables in let expressions. To
-- further illustrate this:
foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- To avoid writing all the lambdas, we have do notation:
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
-- So each of lines is nested within the line above if converted back to
-- >>= notation


-- Do As I Do

-- In a do expression, every line that isn't a let line is a monadic value.
-- We use <- to inspect its result. The last monadic value in a do
-- expression can't be used with <- to bind its result, it wouldn't make sense when translated back to >>= chain.
Just 9 >>= (\x -> Just (x > 8))
-- Just True
marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)
-- With do notation it's easy to see why the result of the monadic
-- expression is the result of the last monadic value.

-- Our tightrope walker's routine can also be expressed in do notation.
routine :: Maybe Pole
routine = do
    start  <- return (0,0)
    first  <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second
-- routine == Just (3,2)
-- unfortunately with do notation we have to explicitly pass the previous
-- Pole to the landLeft/Right functions.

-- Here is the code if we didn't use monadic aspects of Maybe:
routine :: Maybe Pole
routine =
    case Just (0,0) of
        Nothing    -> Nothing
        Just start -> case landLeft 2 start of
            Nothing    -> Nothing
            Just first -> case landRight 2 first of
                Nothing     -> Nothing
                Just second -> landLeft 1 second

-- throwing Pierre a banana:
routine :: Maybe Pole
routine = do
    start  <- return (0,0)
    first  <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second
-- routine == Nothing
--
-- Writing a line in do notation without binding, <-, the monadic value is
-- just like putting >> after the monadic value whose result we want to
-- ignore. It's also prettier than _ <- Nothing.
