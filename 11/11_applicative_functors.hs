-- Functors Redux
--
-- functors are things that can be mapped over like lists, Maybes and trees
-- They are described by the type class Functor which has one type class method
-- fmap
--
-- :t fmap
-- fmap :: (a -> b) -> f a -> f b
--
-- to make a type constructor an instance of Functor, its kind must be
-- * -> *
-- e.g. Maybe
--
-- To make Either into a Functor instance have to write:
--
-- instance Functor (Either a) where
--
-- its type would then be:
-- :t fmap
-- fmap :: (b -> c) -> Either a b -> Either a c


-- IO Actions as Functors
--
-- IO is an instance of Functor
--
-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return (f result)

-- E.g.
main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you said " ++ line' ++ " backwards!"
-- run ./11_backwards_1

-- Rewritten using fmap
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
-- run ./11_backwards_2

-- mapping reverse over getLine gives us an IO Action that gets a line from
-- standad input and applies reverse to its result
-- biding the result to a name using <- gives a value that has already had
-- reverse applied to it.
--
-- If you ever find yourself binding the respult of an IO action to a name,
-- only then to apply a function to that and call it something else, that's
-- a good candidate for using fmap
--
-- Example of applying more than one function to data inside a functor
import Data.Char
import Data.List

main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
-- run ./11_reverse_intersperse


-- Functions as Functors
--
-- Given a function of type a -> b. It can be rewritten as (->) a b.
-- But this takes two parameters and for it to be a functor it has to
-- take just one.
--
-- the implementation in Control.Monad.Instances is as follows:
--
-- instance Functor ((->) r) where
--     fmap f g = (\x -> f (g x))
--
-- recall fmap's type
-- fmap :: (a -> b) -> f a -> f b
--
-- replacing each f with (->) r
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
--
-- now we can write (->) r a and (->) r b as infix
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
--
-- So mapping a function over a function much produce a function.
-- This is function composition!
--
-- Here's another way to write this instance:
--
-- instance Functor ((->) r) where
--     fmap = (.)

import Control.Monad.Instances

a = fmap (*3) (+100) 1
b = (*3) `fmap` (+100) $ 1
c = (*3) . (+100) $ 1
z = fmap (show . (*3)) (+100) 1
-- load 11_composition_as_fmap.hs


-- Again fmap's type:
-- fmap :: (Functor f) => (a -> b) -> f a -> f b
--
-- because Haskell curries functions this can be writen as:
-- fmap :: (a -> b) -> (f a -> f b)
--
-- it takes a function a -> b and produces a function (f a -> f b)
-- This is called Lifting a function
--
-- :t fmap (*2)
-- fmap (*2) :: (Functor f, Num b) => f b -> f b
--
-- :t fmap (replicate 3)
-- fmap (replicate 3) :: Functor f => f a -> f [a]
--
-- fmap (++"!") reverse "hello"
-- "olleh!"

-- fmap can be thought of in two ways:
-- a.  As a function that takes a function and a functor value and then maps that
--     function over the functor value
-- b.  As a function that takes a function and lifts that function so it operates
--     on functor values

import Control.Monad.Instances
-- Reminder
-- instance Functor (Either a) where
--     fmap f (Right x) = Right (f x)
--     fmap f (Left  x) = Left  x

a = fmap (replicate 3) [1,2,3,4]
b = fmap (replicate 3) (Just 4)
c = fmap (replicate 3) (Right "blah")
d = fmap (replicate 3) Nothing
e = fmap (replicate 3) (Left "foo")
-- load 11_fmap_on_replicate.hs


-- Functor Laws

-- Law 1.
-- fmap id == id
--
-- This implies that fmap over the functor value doesn't do anything that is
-- hidden.

-- fmap id (Just 3)
-- Just 3
--
-- id (Just 3)
-- Just 3
--
-- fmap id [1..5]
-- [1,2,3,4,5]
--
-- id [1..5]
-- [1,2,3,4,5]
--
-- fmap id []
-- []
--
-- id []
-- []
--
-- fmap id Nothing
-- Nothing
--
-- id Nothing
-- Nothing
--
-- recall the implementation of fmap for Maybe to see why fmap id == id holds
-- instance Functor Maybe where
--     fmap f (Just x) = Just (f x)
--     fmap f Nothing  = Nothing

-- Law 2.
-- fmap (f . g) x == fmap f (fmap g x)
-- fmap (f . g) x == (fmap f . fmap g) x
--
-- This says that composing two functions then mapping the result function
-- over the functor is the same as mapping one function over a functor then
-- mapping the result over another one
--
-- Using Maybe as an example (Nothing part is trivial due to definition.)
-- fmap (f . g) (Just x)
-- Just ((f . g) x)
-- Just (f (g x))
--
-- fmap f (fmap g (Just x))
-- fmap f (Just (g x))
-- Just (f (g x))


-- Breaking the Law
data CMaybe a = CNothing | CJust Int a
    deriving (Show, Eq) -- C means counter

-- CNothing
--
-- CJust 0 "haha"
--
-- :t CNothing
-- CNothing :: CMaybe a
--
-- :t CJust 0 "haha"
-- CJust 0 "haha" :: CMaybe [Char]
--
-- :t CJust 100 [1,2,3]
-- CJust 100 [1,2,3] :: Num t => CMaybe [t]

instance Functor CMaybe where
    fmap f CNothing          = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)
-- Like the definition for Maybe except the additional increment of counter

-- fmap (++"ha") (CJust 0 "ho")
-- CJust 1 "hoha"
--
-- fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
-- CJust 2 "hohahe"
--
-- fmap (++"blah") CNothing
-- CNothing

-- Does this obey functor laws? We only need one counter example
--
-- fmap id (CJust 0 "haha")
-- CJust 1 "haha"
--
-- id (CJust 0 "haha")
-- CJust 0 "haha"
--
-- load 11_CMaybe.hs

-- CMaybe is part of the functor type class, but since Law 1 doesn't hold
-- it is not a functor!

-- Having functors obey the two laws means you can reason about the code.
-- Mapping over a functor with do nothing more than map.

-- When making your own functors it might be a good idea to through in a
-- few tests to prove that it behaves properly!



-- Using Applicative Functors
--
-- So far we've only mapped functions that take one parameter. What happens
-- if we map a function that requires two parameters?
--
-- :t fmap (*) (Just 3)
-- fmap (*) (Just 3) :: Maybe (Int -> Int)
--
-- this would have a value of Just (3 *)
-- i.e. a function wrapped in a just
--
-- to use it however... is a bit messy
-- fmap (\f -> f 3) $ fmap (*) (Just 3)
-- Just 9
--
-- other examples:
--
-- :t fmap (++) (Just "hey")
-- fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
--
-- fmap (\f -> f " how're you?") $ fmap (++) (Just "hey")
-- Just "hey how're you?"
--
-- :t fmap compare (Just 'a')
-- fmap compare (Just 'a') :: Mabye (Char -> Ordering)
--
-- fmap (\f -> f 'z') $ fmap compare (Just 'a')
-- Just LT
--
-- :t fmap compare "A LIST OF CHARS"
-- fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
--
-- fmap (\f -> f 'A') $ fmap compare "A LIST OF CHARS"
-- [EQ,LT,GT,GT,GT,GT,LT,GT,GT,LT,GT,GT,EQ,GT,GT]
--
-- :t fmap (\x y z -> x + y / z) [3,4,5,6]
-- fmap (\x y z -> x + y / z) [3,4,5,6] :: Fractional a => [a -> a -> a]
--
-- fmap (\z -> z 2) $ fmap (\y -> y 1) $ fmap (\x y z -> x + y / z) [3,4,5,6]
-- [3.5, 4.5, 5.5, 6.5]


-- With normal functors we can't map Just (3 *) over (Just 5)
-- we can't map functions that are inside functor values over another
-- functor value. This is where applicative functors come in.


-- Say Hello to Applicative

-- the Applicative type class defines two functions:
class (Functor f) => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-- no default implementation is provided, so have to make your own if you
-- define a class as an applicative functor

-- firstly, if something wants to be Applicative, it has to be an instance of
-- Functor first. I.e. we can use fmap on it.
--
-- pure takes any type a and wraps it inside f. Like putting a value into a
-- default (or pure) context
--
-- <*> is like fmap, but more powerful.
-- It takes a functor value with a function inside it and a functor value
-- then applies the function over the value in second functor


-- Maybe the Applicator Functor
--
-- the applicative instance implementation for Maybe
instance Applicative Maybe where
    pure = Just
    Nothing  <*> _         = Nothing
    (Just f) <*> something = fmap f something

-- pure wraps something in an applicative value, hence Just.
-- There is no function inside Nothing, so we return Nothing.
-- Otherwise we extract f and map f over something with fmap.

-- examples
import Control.Applicative

Just (+3) <*> Just 9
-- Just 12
pure (+3) <*> Just 10
-- Just 13
pure (+3) <*> Just 9
-- Just 12
Just (++"hahah") <*> Nothing
-- Nothing
Nothing <*> Just "woot"
-- Nothing

-- Use "pure" when you are dealing with values in an applicative context,
-- rather than a raw Just.
--
-- These examples could easily have been achieved with mapping unwrapped
-- functions over functors like we did before: fmap (\f -> ... ) something
--
-- With normal functors mapping a function over a functor, you can't really
-- get the result out in any general way. Applicative functors allow you to
-- operate on several functor with a single function


-- The Applicative Style

pure (+) <*> Just 3 <*> Just 5
-- Just 8
pure (+) <*> Just 3 <*> Nothing
-- Nothing
pure (+) <*> Nothing <*> Just 5
-- Nothing

-- <*> is left associative, so
pure (+) <*> Just 3 <*> Just 5
(pure (+) <*> Just 3) <*> Just 5
(Just (+) <*> Just 3) <*> Just 5
Just (3+) <*> Just 5
Just 8
-- applicative functors and applicative style allow us to apply functions that
-- don't expect applicative values and apply to values that are applicative
--
-- since pure f <*> x == fmap f x, we don't really need to wrap the function
-- in the first place, so Control.Applicative also exports <$>

(<$>)  :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- now we can instead write
(++) <$> Just "johntra" <*> Just "volta"
-- Just "johntravolta"


-- Lists

-- The list type constructor [] is an applicative functor
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- pure creates a minimal context that can yield a value
-- [] and Nothing do not contain values.
pure "Hey" :: [String]
-- ["Hey"]
pure "Hey" :: Maybe String
-- Just "Hey"
[(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]
[(+),(*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]
(++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

-- list could be viewed as non-deterministic computations
-- values like 100 or "what" can be viewed as deterministic: only one result
-- [1,2,3] can be viewed as a computation that can't decide, so gives all the
-- possible results. (+) <$> [1,2,3] <*> [4,5,6] is like adding two
-- non-deterministic computations and getting something back that is even
-- less sure about the result :)

[x*y | x <- [2,5,10], y <- [8,10,11]]
-- [16,20,22,40,50,55,80,100,110]
(*) <$> [2,5,10] <*> [8,10,11]
-- [16,20,22,40,50,55,80,100,110]
filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
-- [55,80,100,110]
