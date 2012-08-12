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
