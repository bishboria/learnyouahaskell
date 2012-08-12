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

For instance, we might want to equip our values with strings that explain what's going on, probably for debugging purposes.

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
