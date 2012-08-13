FOR A FEW MONADS MORE

In this chapter we'll cover a few more monads and also see how to make
programs clearer by using them.

The monads we'll be covering are part of the mtl package (ghc-pkg list).


Writer? I Hardly Knew Her!

We've already seen Maybe, [] and IO monads, now let's see the Writer monad.
While Maybe is for values with the added context of failure, and list monad
is for nondeterministic values, the Writer monad is for values that have
another value attached that acts simliar to a log.

Writer allows us to do computations while making sure that all log values
are combined into one log value, which is then attached to the result.

For instance, we might want to equip our values with strings that explain
what's going on, probably for debugging purposes.

Consider: a function that takes a number of bandits in a gang and tells us
if that's a big gang...

> isBigGang  :: Int -> Bool
> isBigGang x = x > 9

What if we wanted to also return a log string that says what it did?

> isBigGang  :: Int -> (Bool, String)
> isBigGang x = (x > 9, "Compared gang size to 9.")

> isBigGang 3
<>(False, "Compared to gang size to 9.")
> isBigGang 30
<>(True,"Compared gang size to 9.")

isBigGang takes a value and returns a value with a context. Feeding it a
normal value is fine. What if we have a value with a context?

When dealing with Maybe, we made a function applyMaybe. In the same vein,
let's make an applyLog function. It will make sure that the log of the
original value is not lost when computing a new value.

> applyLog           :: (a, String) -> (a -> (b, String)) -> (b, String)
> applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

> (3, "Smallish gang.") `applyLog` isBigGang
<>(False,"Smallish gang.Compared gang size to 9.")
> (30, "A freaking platoon.") `applyLog` isBigGang
<>(True,"A freaking platoon.Compared gang size to 9.")
> ("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
<>(5,"Got outlaw name.Applied length.")
> ("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))
<>(7,"Got outlaw name.Applied length.")


Monoids to the Rescue

Rigt now, applyLog takes values of type (a, String), but there is no reason
that it can't be a list of any kind. We can change its type to:

> applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])

But, applyLog could also work for ByteStrings and the type we have only
works for lists... However, both lists and bytestrings are monoids and as
such both implements mappend. And on both lists and bytestring, mappend is
for appending.

> [1,2,3] `mappend` [4,5,6]
<>[1,2,3,4,5,6]
> import qualified Data.ByteString as B
> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]
<>"chihuahua"

So now we need to change applyLog to:

> applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
> applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

And it can work on any monoid. We no longer need to think about the tuple
as a value and a log; now we can think of a value combined with a monoid
value.

We could try to keep track of food and prices:

> import Data.Monoid
>
> type Food  = String
> type Price = Sum Int
>
> addDrink        :: Food -> (Food, Price)
> addDrink "beans" = ("milk", Sum 25)
> addDrink "jerky" = ("whiskey", Sum 99)
> addDrink _       = ("beer", Sum 30)

As a reminder:

> Sum 3 `mappend` Sum 9
<>Sum 12

addDrink in use:

> ("beans", Sum 10) `applyLog` addDrink
<>("milk", Sum {getSum = 35})
> ("jerky", Sum 25) `applyLog` addDrink
<>("whiskey", Sum {getSum = 124})
> ("dogmeat", Sum 5) `applyLog` addDrink
<>("beer", Sum {getSum = 35})

When we were doing logs, the attached value was a string that got appended.
With Sum a now we are attaching values that keeps a number that gets added.

> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink
<>("beer",Sum {getSum = 65})


The Writer Type

Now that we've seen a value with an attached monoid acts like a monad, let's
see the Monad instance (in Control.Monad.Writer)

> newtype Writer w a = Writer { runWriter :: (a, w) }

Control.Monad.Writer doesn't export the value constructor, so that it can
change the implementation whenever it wishes. It exports the writer
function.

Because the constructor isn't exported, you can't pattern match against it.
You need to use runWriter instead to get the tuple out of the monad.

> instance (Monoid w) => Monad (Writer w) where
>     return x = Writer (x, mempty)
>     (Writer (x, v)) >>= f = let (Writer (y, v')) = f x
>                             in Writer (y, v `mappend` v')

It is easy to see that >>= is the same as applyLog except wrapping values
in a Writer. return is setting up the value in a minimum context. Since we
are using monoids as the attached value, the minimum context for it is
mempty.

Let's return 3 a few times with different monoids:

> runWriter (return 3 :: Writer String Int)
<>(3,"")
> runWriter (return 3 :: Writer (Sum Int) Int)
<>(3,Sum {getSum = 0})
> runWriter (return 3 :: Writer (Product Int) Int)
<>(3,Product {getProduct = 1})

Writer instance doesn't have its own implementation of fail, so it will
call error if it fails to pattern match.


Using do Notation with Writer

Because we now have a Monad instance (with Writer), we can use do notation

> import Control.Monad.Writer
>
> logNumber  :: Int -> Writer [String] Int
> logNumber x = writer (x, ["Got number: " ++ show x])
>
> multWithLog :: Writer [String] Int
> multWithLog = do
>     a <- logNumber 3
>     b <- logNumber 5
>     return (a*b)

We use return to present the final value as it will put a*b in a default
context and not add anything new to the log.

> runWriter multWithLog
<>(15,["Got number: 3","Got number: 5"])

Sometimes it's useful just to be able to add some monoid value whenever we
want, so we use tell

> multWithLog :: Writer [String] Int
> multWithLog = do
>     a <- logNumber 3
>     b <- logNumber 5
>     tell ["Gonna multiply these two"]
>     return (a*b)
>
> runWriter multWithLog
<>(15,["Got number: 3","Got number: 5","Gonna multiply these two"])

tell takes a monoid value and creates a Write value that returns the dummy
() value as its result but has the desired monoid attached. When we have a
monadic calue that has () as its result, we don't bind it to a variable.

Remember the final line in do notation is the result of the entire
expression.


Adding Logging to Programs

Euclid's algorithm takes two numbers and calculates the greatest common
divisor. Haskell already comes with gcd, but let's make our own that has
logging capabilities.

first the standard algorithm
> gcd' :: Int -> Int -> Int
> gcd' a b
>     | b == 0    = a
>     | otherwise = gcd' b (a `mod` b)

> gcd' 8 3
<>1

Now we want to equip it with with a context. A monoid that acts as a log.
> import Control.Monad.Writer
>
> gcd' :: Int -> Int -> Writer [String] Int
> gcd' a b
>     | b == 0 = do
>         tell ["Finished with " ++ show a]
>         return a
>     | otherwise = do
>         tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
>         gcd' b (a `mod` b)

> fst $ runWriter (gcd' 8 3)
<>1
> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
<>8 mod 3 = 2
<>3 mod 2 = 1
<>2 mod 1 = 0
<>Finished with 1

By changing normal values to monadic values we are change the algorithm to
report what it's doing. We also let the implementation of >>= for Writer
take care of the logs for us. To add a logging mechanism to a function:
Replace normal values with Writer values, change normal function application
with >>= (or do expression).


Inefficient List Construction

When using the Writer monad, you need to be careful to choose the correct
monoid, lists can turn out to be very slow.

List use ++ for mappend and adding something to the end of the list is slow
if that list is really long.

In gcd' we something like:
<>a ++ (b ++ (c ++ (d ++ e)))
This is efficient as we're constructing the left part of the list and adding
more on the right.

If not careful, it can turn out like:
<>(((a ++ b) ++ c) ++ d) ++ e
This is bad because it associates to the left instead and must construct
the left part all the way to the beginning .

The following version of gcd logs in reverse

> import Control.Monad.Writer
> 
> gcdReverse :: Int -> Int -> Writer [String] Int
> gcdReverse a b
>     | b == 0 = do
>         tell ["Finished with " ++ show a]
>         return a
>     | otherwise = do
>         result <- gcdReverse b (a `mod` b)
>         tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
>         return result

It does the recursion first and binds its resulting value to result. It then
adds the current step to the log, but at the end. Finally it returns the
result at the end.

> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
<>Finished with 1
<>2 mod 1 = 0
<>3 mod 2 = 1
<>8 mod 3 = 2

This is inefficient as it ends up associating the use of ++ to the left
instead of to the right. Because lists can be inefficient when repeatedly
appending like this, it's best to use a data structure that always supports
effiicent appending. Like the difference list.
