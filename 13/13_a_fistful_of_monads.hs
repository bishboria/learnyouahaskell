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

-- When to use do and when to use >>= is up to you. This example works best
-- with >>= as the each step specifically relies on the previous result.
-- Using do, we have to bind each result and use that in the next...


-- Pattern Matching and Failure

-- In do notation we can utilize pattern matching when we bind monadic
-- values to names.
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x
-- justH == Just 'h'
--
-- What if this pattern were to fail? In functions, the next pattern is
-- tried, until one is matched or an error is thrown and the program crashes
--
-- A failed pattern match in a let expression results in an error straight
-- away.
--
-- In do notation, the fail method (on the Monad type class) enables it to
-- result in a failure in the context of the current monad, instead of
-- making the program crash.
--
-- its default implementation:
fail :: (Monad m) => String -> m a
fail msg = error msg
-- although, this default impl does cause the program to crash...

-- Maybe is implemented thusly:
fail _ = Nothing
-- it ignores the error message and returns Nothing so the final value in
-- the sequence of computations will be Nothing

-- Here is a do expression with a pattern match that will fail:
wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x
-- wopwop == Nothing
-- The failed pattern match has caused a failure within the context of the
-- monad without crashing the entire program.


-- The List Monad

-- Using lists as applicative functors:
(*) <$> [1,2,3] <*> [10,100,1000]
-- [10,100,1000,20,200,2000,30,300,3000]

-- how [] is defined as a monad
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _   = []

[3,4,5] >>= \x -> [x,-x]
-- [3,-3,4,-4,5,-5]

-- The empty list is pretty much the equivalent of Nothing in Maybe
[] >>= \x -> ["bad","mad","rad"]
-- []
[1,2,3] >>= \x -> []
-- []

-- Just as with Maybe, we can chain them up:
[1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch)
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- The equivalent using do notation
listOfTuples :: [(Int,Char)]
listOfTuples  = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- The above makes it a bit more obvious that n takes on every value of
-- [1,2], same for ch.


-- do Notation and List Comprehensions

-- using lists with do notation might remind you of something
[ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- It turns out list comprehensions are just syntactic sugar for using lists
-- as monads.


-- MonadPlus and the guard Function

-- List comprehensions allow us to filter output
[ x | x <- [1..50], '7' `elem` show x ]
-- [7,17,27,37,47]
-- To see how filtering in list comprehensions translates to list monad we
-- need to look at the guard function on the MonadPlus type class.

-- MonadPlus type class is for monads that can also act as monoids.
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
-- mzero is synonymous with mempty from Monoid, and mplus corresponds to
-- mappend

-- Lists are monoids as well as monads so they can be MonadPlus too:
instance MonadPlus [] where
    mzero = []
    mplus = (++)
-- mzero for lists is failure. mplus joins two values into one.

guard      :: (MonadPlus m) => Bool -> m ()
guard True  = return ()
guard False = mzero
-- If guard receives True, it returns an empty tuple, (), and puts it into
-- a minimum default context that still succeeds. False returns [] meaning
-- failure.
guard (5 > 2) :: Maybe ()
-- Just ()
guard (1 > 2) :: Maybe ()
-- Nothing
guard (5 > 2) :: [()]
-- [()]
guard (1 > 2) :: [()]
-- Nothing

-- we can use guard like so:
[1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- [7,17,27,37,47]
-- Which is the same as the list comprehension above

guard (5 > 2) >> return "cool" :: [String]
-- ["cool"]
guard (1 > 2) >> return "cool" :: [String]
-- []
-- If the guard succeeds the result contained within it is the empty tuple.
-- Then >> ignores the empty tuple and returns something else as the result.
-- If the guard fails, then so does the return. feeding [] to >>= is
-- always a failure.
--
-- If the Bool is False, produce a failure now. If it is True, make a
-- succesful value and put the dummy result () inside it.
--
-- This allows the computation to continue.

-- In do notation:
sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x
-- sevensOnly == [7,17,27,37,47]
--
-- filtering in list comprehensions is the same as using guard


-- A Knight's Quest

-- We want to find out if a Knight can reach a certain position on a
-- chessboard in three moves.
type KnightPos = (Int,Int) -- Column, Row
-- since we have nondeterminism, use of lists, at our disposal let's pick
-- all the moves at once instead of picking one.

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')
-- Given a starting position (c,r), a knight can only take L shaped moves
-- of the form in the list being drawn from. The guard ensures all moves
-- are still on the board.

-- Alternative implementation
moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

moveKnight (6,2)
-- [(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]
moveKnight (8,1)
-- [(6,2),(7,3)]

-- Here's a function that takes a position and works out all possible
-- positions in three moves.
in3 :: KnightPos -> [KnightPos]
in3 start = do
    first  <- moveKnight start
    second <- moveKnight first
    moveKnight second

in3 (6,2)
-- [(8,1),(8,3),(4,1),(4,3),(7,4),(5,4),(5,2),(5,4),(8,1),(8,5),(6,1),(6,5)
-- ,(8,1),(8,3),(4,1),(4,3),(7,4),(5,4),(8,3),(8,5),(4,3),(4,5),(7,2),(7,6)
-- ,(5,2),(5,6),(5,2),(8,3),(6,3),(5,4),(5,6),(8,3),(8,7),(6,3),(6,7),(8,1)
-- ,(8,3),(4,1),(4,3),(7,4),(5,4),(4,1),(4,3),(3,4),(1,4),(7,2),(7,4),(3,2)
-- ,(3,4),(6,1),(6,5),(4,1),(4,5),(5,2),(5,4),(1,2),(1,4),(4,1),(4,5),(2,1)
-- ,(2,5),(8,1),(8,3),(4,1),(4,3),(7,4),(5,4),(8,3),(8,5),(4,3),(4,5),(7,2)
-- ,(7,6),(5,2),(5,6),(4,1),(4,3),(3,4),(1,4),(4,3),(4,5),(3,2),(3,6),(1,2)
-- ,(1,6),(7,2),(3,2),(6,3),(4,3),(7,4),(7,6),(3,4),(3,6),(6,3),(6,7),(4,3)
-- ,(4,7),(5,2),(1,2),(4,3),(2,3),(5,4),(5,6),(1,4),(1,6),(4,3),(4,7),(2,3)
-- ,(2,7),(7,2),(7,4),(3,2),(3,4),(6,1),(6,5),(4,1),(4,5),(7,4),(7,6),(3,4)
-- ,(3,6),(6,3),(6,7),(4,3),(4,7),(6,1),(6,3),(7,4),(6,5),(6,7),(7,4),(7,8)
-- ,(8,1),(8,3),(4,1),(4,3),(7,4),(5,4),(8,5),(8,7),(4,5),(4,7),(7,4),(7,8)
-- ,(5,4),(5,8),(5,2),(5,4),(8,1),(8,5),(6,1),(6,5),(5,4),(5,6),(8,3),(8,7)
-- ,(6,3),(6,7),(5,2),(5,4),(1,2),(1,4),(4,1),(4,5),(2,1),(2,5),(5,4),(5,6)
-- ,(1,4),(1,6),(4,3),(4,7),(2,3),(2,7),(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)
-- ,(8,5),(8,7),(4,5),(4,7),(7,4),(7,8),(5,4),(5,8),(6,1),(6,3),(2,1),(2,3)
-- ,(5,4),(3,4),(6,5),(6,7),(2,5),(2,7),(5,4),(5,8),(3,4),(3,8)]
-- The same position can appear several times as there are multiple ways to
-- reach the same position.

-- same code without do notation
in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight
-- we could do moveKnight start instead of return start >>= moveKnight...

-- Finally, a function that takes two positions and tells us if we can reach
-- that position in 3 moves.
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

canReachIn3 (6,2) (6,1)
-- True
canReachIn3 (6,2) (7,3)
-- False

-- Change this function to show the list of moves if you can reach a place
-- in 3 moves
