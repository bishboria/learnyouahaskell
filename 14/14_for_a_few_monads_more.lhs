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


Using Difference Lists

A difference list is a function that takes a list and prepends another list
to it. For example, the difference list equivalent of [1,2,3] is the
function \xs -> [1,2,3] ++ xs. An empty difference list is \xs -> [] ++ xs

Appending two difference lists can be done like:
> f `append` g = \xs -> f (g xs)
Where f and g are functions that take lists and prepend something to them
> f = ("dog"++)
> g = ("meat"++)
> f `append` g == \xs -> "dog" ++ ("meat" ++ xs)

> newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
>
> toDiffList :: [a] -> DiffList a
> toDiffList xs = DiffList (xs++)
>
> fromDiffList :: DiffList a -> [a]
> fromDiffList (DiffList f) = f []

Here is the monoid instance:
> instance Monoid (DiffList a) where
>     mempty = DiffList (\xs -> [] ++ xs)
>     (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

Notice how mempty is just id. And mappend is just function composition.

> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])
<>[1,2,3,4,1,2,3]

Now we can increase the efficiency of gcdReverse by making it use difference
lists instead.

> gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
> gcdReverse' a b
>     | b == 0 do
>         tell (toDiffList ["Finished with " ++ show a]
>         return a
>     | otherwise = do
>         result <- gcdReverse' b (a `mod` b)
>         tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
>         return result
We need to change the Writer type to DiffList String instead of [String]
and also intercept each argument to tell and turn it into a DiffList first

> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse' 100 34
<>Finished with 2
<>32 mod 2 = 0
<>34 mod 32 = 2
<>100 mod 34 = 32


Comparing Performance

To get a feel for how much difference lists improve performance, let's
count down from some number to zero but produce the log in reverse to that
the log counts up.

> finalCountDownFast :: Int -> Writer (DiffList String) ()
> finalCountDownFast 0 = do
>     tell (toDiffList ["0"])
> finalCountDownFast x = do
>     finalCountDownFast (x-1)
>     tell (toDiffList [show x])
>
> finalCountDownSlow :: Int -> Writer [String] ()
> finalCountDownSlow 0 = do
>     tell ["0"]
> finalCountDownSlow x = do
>     finalCountDownSlow (x-1)
>     tell [show x]
>
> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDownFast 500000
> mapM_ putStrLn . snd . runWriter $ finalCountDownSlow 500000

Running each of the two shows that finalCountDownFast is a lot quicker at
producing output.

*In fact, finalCountDownSlow ran out of memory on my machine and ghci
terminated... it only reached 256... finalCountDownFast easily reaches the
end*

This isn't scientific way of testing performance, but is a quick way to see
that difference lists have benefits over ordinary lists


Reader? Ugh, Not This Joke Again

In chapter 11 we saw that (->) r is an instance of Functor. Mapping a
function f over g will make a function like: \x -> f (g x)
> let f = (*5)
> let g = (+3)
> (fmap f g) 8
<>55

We also saw that functions are Applicative Functors. They allow us to
operate on the eventual results of functions as if we already had their
results.
> let f = (+) <$> (*2) <*> (+10)
> f 3
<>19


Functions As Monads

(->) r is also a monad. A function can be considered a value with a context.
The context is that the value is not present yet and we need to apply that
function to something in order to get the resut

> instance Monad ((->) r) where
>     return x = \_ -> x
>     h >>= f  = \w -> f (h w) w

Bind looks a bit cryptic here: Because we are using functions as monads, we return \w ... from >>=. To get the result from a function we need to apply
it to h first, hence (h w). The result is then passed to f which returns a
monadic value (a function in this case) and so we apply that to w as well.


The Reader Monad

Here's a do expression showing >>= in action
> import Control.Monad.Instances
>
> addStuff :: Int -> Int
> addStuff = do
>     a <- (*2)
>     b <- (+10)
>     return (a+b)
>
> addStuff 3
<>19

This is the same as the applicative version above. Written as >>=
> import Control.Monad.Instances
>
> addStuff :: Int -> Int
> addStuff = (*2)  >>= \a ->
>            (+10) >>= \b ->
>            return (a+b)
>
> addStuff 3
<>19

Both (*2) and (+10) are applied to the number 3 (in this case). return (a+b)
does aswell, but it ignores that value and always presents a+b as the
result. It's called the reader monad as all functions read from a common
source.

addStuff could also be written like, which might make it clearer:
> addStuff :: Int -> Int
> addStuff x = let
>     a = (*2) x
>     b = (+10) x
>     in a+b

The Reader monad allows us to treat functions as values with a context. We
can act like we already know what the function will return. If we have lots
of functions that are all just missing one parameter and they will
eventually be applied to the same thing, we can use the reader monad to
extract their future results and >>= will make sure it works out.


Tasteful Stateful Computations

Consider the threeCoins function from Chapter 9:
> threeCoins :: StdGen -> (Bool,Bool,Bool)
> threeCoins gen =
>     let (firstCoin, newGen)   = random gen
>         (secondCoin, newGen') = random newGen
>         (thirdCoin, newGen'') = random newGen'
>     in  (firstCoin, secondCoin, thirdCoin)
Here we are passing state between calls to random. We have to do this since
Haskell is pure. Handling state manually like this is a bit tedious so
Haskell uses the State Monad.


Stateful Computations

A stateful computation is a function that takes some state and returns a
value along with some new state:
<> s -> (a, s)


Stacks and Stones

If we want to model a stack we need to:
    push: an element on to the top of the stack
    pop:  removes the topmost element from the stack

Here are the functions in use:
> type Stack = [Int]
>
> pop :: Stack -> (Int, Stack)
> pop (x:xs) = (x, xs)
>
> push :: Int -> Stack -> (Int, Stack)
> push a xs = ((), a:xs)

Pop is a stateful computation because of its type. Applying the first
parameter to push makes it into a stateful computation. Here's some code
to test:

> stackManip :: Stack -> (Int, Stack)
> stackManip stack = let
>     ((), newStack1) = push 3 stack
>     (a, newStack2)  = pop newStack1
>     in pop newStack2

We take a stack, push 3 onto it, then pop twice (discarding the value of the
first pop, a).

> stackManip [5,8,2,1]
<>(5,[8,2,1])

Notice that stackManip is itself a stateful computation. So we've taken some
stateful computations and glued them together into another stateful
computation...

Using the State Monad allows us to do stackManip in the following way:
> stackManip = do
>     push 3
>     a <- pop
>     pop
So we won't need to deal with intermediate state manually.


The State Monad

Control.Monad.State module provides a newtype that wraps stateful
computations:

> newtype State s a = State { runState :: s -> (a, s) }

Control.Monad.State like Control.Monad.Writer doesn't export its
constructor, only the state function. This function takes a stateful
computation.

Here is the Monad instance:

> instance Monad (State s) where
>     return x = State $ \s -> (x, s)
>     (State h) >>= f = State $ \s -> let (a, newState) = h s
>                                         (State g)     = f a
>                                     in  g newState

return takes a value and puts it in a stateful computation that always has
that value as the result.

What about >>=? The result of feeding a stateful computation to a function
with >>= must itself, so we start out with declaring a lambda. The lambda
will be the result. We apply state s to h, which gives us a value and a
newState. We pass the value a to f and that produces the next state that we
can apply newState to. Giving the result. The stateful computation g is
hidden inside f and requires a to get it out.

To see how this works out translate:
> stackManip = do
>     push 3
>     pop
>     push 6
into >>= notation

It's easy to change push and pop using a state wrapper as they are already
stateful computations

> import Control.Monad.State
>
> pop :: State Stack Int
> pop = state $ \(x:xs) -> (x, xs)
>
> push :: Int -> State Stack ()
> push a = state $ \xs -> ((), a:xs)
>
> stackManip :: State Stack Int
> stackManip = do
>     push 3
>     a <- pop
>     pop
>
> runState stackManip [5,8,2,1]
<>(5,[8,2,1])

We didn't need to bind pop to a, as we don't use it later, so we could
rewrite it as:
> stackManip :: State Stack Int
> stackManip = do
>     push 3
>     pop
>     pop

Given stackManip's type we must always have pop as the last operation.

Something a bit more complicated:
> stackStuff :: State Stack ()
> stackStuff = do
>     a <- pop
>     if a == 5
>         then push 5
>         else do
>             push 3
>             push 8
>
> runState stackStuff [9,0,2,1,0]
<>((),[8,3,0,2,1,0])

Because do expressions result in monadic values. do expressions with State
monads are also stateful computations. So that means we can glue together
stackManip and stackStuff to produce more stateful computations:

> moreStack :: State Stack ()
> moreStack = do
>     a <- stackManip
>     if a = 100
>         then stackStuff
>         else return ()
>
> runState moreStack [8,3,2,1]
<>((),[3,2,1])
> runState moreStack [100,4,2,1]
<>((),[8,3,2,1])
> runState moreStack [100,5,3,2,1]
<>((),[5,3,2,1]


Getting and Setting State

Control.Monad.State module provides a type class called MonadState, which
has two useful functions: get and put. for State:

> get = state $ \s -> (s, s)
It takes the current state and presents it as the result.

> put newState = state $ \s -> ((), newState)
Takes some state and creates a stateful function that replaces the current
state with it.

With these functions we can see what the current stack is and we can replace
it with another stack.

> stackyStack :: State Stack ()
> stackyStack = do
>     stackNow <- get
>     if stackNow == [1,2,3]
>         then put [8,3,1]
>         else put [9,2,1]

We can also use get and put to implement push and pop:

> pop :: State Stack Int
> pop = do
>     (x:xs) <- get
>     put xs
>     return x
>
> push :: Int -> State Stack ()
> push x = do
>     xs <- get
>     put (x:xs)

If >>= only worked with State values then:

> (>>=) :: State s a -> (a -> State s b) -> (State s b)

State s stays the same type, but a can change into b. This means we can glue
together several statements whose results are of different types, but
state's type must stay the same. State s is the monad.


Randomness and sthe State Monad

Earlier we spoke of how generating random numbers can be awkward due to
passing of the new generator as state. This is simplified with the State
monad.

Recall:
> random :: (RandomGen g, Random a) => g -> (a, g)

We take a random generator and produce a random number along with a new
generator. This is a stateful computation.

> import System.Random
> import Control.Monad.State
>
> randomSt :: (RandomGen g, Random a) => State g a
> randomSt = state random

So, now if we want to throw three coins:

> threeCoins :: State StdGen (Bool, Bool, Bool)
> threeCoins = do
>     a <- randomSt
>     b <- randomSt
>     c <- randomSt
>     return (a, b, c)

threeCoins is now a stateful computation

> runState threeCoins (mkStdGen 33)
<>((True,False,True),680029187 2103410263)

Now doing things that require state is much less painful.


Error Error on the Wall

Maybe is used to add a context of possible failure to values. Although, it
doesn't say how/why it failed. This is where Either comes in. Either is an
enhanced Maybe. It's implementation is found in Control.Monad.Error:

> instance (Error e) => Monad (Either e) where
>     return x       = Right x
>     Right x  >>= f = f x
>     Left err >>= f = Left err
>     fail msg       = Left (strMsg msg)

This instance has the extra requirement that e, in Either e, must be an
instance of the Error type class. This is for types that can act like error
messages. strMsg function takes an error in the form of a string and returns
a value such that can be interpreted as an instance of Error.

String is an instance of Error

> :t strMsg
<>strMsg :: (Error a) => String -> a
> strMsg "boom!" :: String
<>"boom!"

As we use String to describe the error when using Either, we don't worry
about this too much.

Examples:
> Left "boom" >>= \x -> return (x+1)
<>Left "boom"
> Left "boom" >>= \x -> Left "no way!"
<>Left "boom"
> Right 100 >>= \x -> Left "no way!"
<>Left "no way!"
> Right 3 >>= \x -> return (x + 100)
<>Right 103

Sometimes the last example may have a type error (although mine didn't...)
If it does, do this:
> Right 3 >>= \x -> return (x + 100) :: Either String Int
Since the error type is ambiguous (it's never used or declared).

As a task, change the tightrope walker function to use the Error monad so
that when Pierre slips and falls, we see how many birds were on each side
when he fell.


Some Useful Monadic Functions

We'll look at the following monadic functions: liftM, join, filterM and
foldM


liftM and Friends

liftM takes a function and a monadic value and maps the function over the
monadic value. Which is the same as fmap on Functor

> liftM :: (Monad m)   => (a -> b) -> m a -> m b
> fmap  :: (Functor f) => (a -> b) -> f a -> f b

If the Functor and Monad instances for a type obey the functor and monad
laws, they amount to the same thing.

> liftM (*3) (Just 8)
<>Just 24
> fmap (*3) (Just 8)
<>Just 24
> runWriter $ liftM not $ writer (True, "chickpeas")
<>(False,"chickpeas")
> runWriter $ fmap not $ writer (True, "chickpeas")
<>(False,"chickpeas")
> runState (liftM (+100) pop) [1,2,3,4]
<>(101,[2,3,4])
> runState (fmap (+100) pop) [1,2,3,4]
<>(101,[2,3,4])

liftM applied to a Writer applies the function to the first part of the
tuple, which is the result.

> liftM    :: (Monad m ) => (a -> b) -> m a -> m b
> liftM f m = m >>= (\x -> return (f x))

Or with do notation:

> liftM    :: (Monad m ) => (a -> b) -> m a -> m b
> liftM f m = do
>     x <- m
>     return (f x)

We can implement fmap, or liftM, just by using monadic properties.

Applicative type class allows to to apply functions between values with
contexts as if they were normal values.

> (+) <$> Just 3 <*> Just 5
<>Just 8
> (+) <$> Just 3 <*> Nothing
<>Nothing

<$> is just fmap.

> (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

So <*> is kind of like fmap, except that the function is in a context. <*>
can also be implemented using monadic properties. ap function is basically
<*> but with a Monad constraint instead of an Applicative constraint.

> ap :: (Monad m) => m (a -> b) -> m a -> m b
> ap mf m = do
>     f <- mf
>     x <- m
>     return (f x)

ap takes a function that's wrapped in a monad and a monadic value. It
extracts the function out of the monad, f, and the value, x. It applies f to
x and puts it in a minimal context.

> Just (+3) <*> Just 4
<>Just 7
> Just (+3) `ap` Just 4
<>Just 7
> [(+1),(+2),(+3)] <*> [10,11]
<>[11,12,12,13,13,14]
> [(+1),(+2),(+3)] `ap` [10,11]
<>[11,12,12,13,13,14]

If you have a Monad instance you can make it Applicative by saying that
pure is return and <*> is `ap`. Similarly, if you already have a Monad, you
can make it a Functor by saying that fmap is liftM.

We learned from Chapter 11 that:
> liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
> liftA2 f x y = f <$> x <*> y

liftM2 does the same thing, except it has a Monad constraint instead of an
Applicative constraint.


The Join Function

If the result of one monadic value is another monadic value, can we flatten
it to get just a single monadic value back? E.g. Just (Just 9)

> join :: (Monad m) => m (m a) -> m a
>
> join (Just (Just 9))
<>Just 9
> join (Just Nothing)
<>Nothing
> join Nothing
<>Nothing
> join [[1,2,3],[4,5,6]]
<>[1,2,3,4,5,6]
For lists, join is just concat.

> runWriter $ join (writer (writer (1, "aaa"), "bbb"))
<>(1,"bbbaaa")
The out monoid comes first, then the inner monoid is appended to it.

> join (Right (Right 9)) :: Either String Int
<>Right 9
> join (Right (Left "error")) :: Either String Int
<>Left "error"
> join (Left "error") :: Either String Int
Flattening Either is similar to Maybe.

> runState (join (state $ \s -> (push 10, 1:2:s))) [0,0,0]
<>((),[10,1,2,0,0,0])
Applying join to a stateful computation whose result is also a stateful
computation results in applying the outer first then the inner. The lambda
pushes 2 then 1 onto the stack given to it. push 10 is then carried out.

The implemenation of join is as follows:
> join :: (Monad m) => m (m a) -> m a
> join mm = do
>     m <- mm
>     m
The use of <- handles the monadic context of the resultant m.

An interesting property of join is that:
> m >>= f == join (fmap f m)
Also, it's often easier to figure out how to flatten a nested monad than to
figure out how to implement >>=...

Another interesting thing is that neither Functors nor Applicatives can
provide the functions required in order to implement join. So that means
Monads are stronger than both Functors and Applicatives as it can do more.


filterM

The filter function's type is:
> filter :: (a -> Bool) -> [a] -> [a]

What if the Bool returned was a monad and came with a context? If we didn't
attach the context to the new output, then the context will be lost.

filterM's type:
> filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

The predicate returns a monadic value whose result is a Bool, but because
it can have a context that could be anything.

Using filter:
> filter (\x -> x < 4) [9,1,5,2,10,3]
<>[1,2,3]

Now a predicate that also provides a log of everything that it did:
> keepSmall :: Int -> Writer [String] Bool
> keepSmall x
>     | x < 4 = do
>         tell ["Keeping " ++ show x]
>         return True
>     | otherwise = do
>         tell [show x ++ " is too large, throwing it away"]
>         return False

Instead of returning a Bool, this returns a Writer [String] Bool.

> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
<>[1,2,3]
> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
<>9 is too large, throwing it away
<>Keeping 1
<>5 is too large, throwing it away
<>Keeping 2
<>10 is too large, throwing it away
<>Keeping 3

A cool trick in Haskell is the ability to get the powerset of a list using
filterM. It uses the nondeterministic nature of list monads.

> powerset :: [a] -> [[a]]
> powerset xs = filterM (\x -> [True, False]) xs
>
> powerset [1,2,3]
<>[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

How is it so easy to produce? The secret is in the implementation of filterM
> filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
> filterM p (x:xs) = do
>    flg <- p x
>    ys <- filterM p xs
>    if flg then x:ys else ys

The recursive call and the usage of each value of flg make creating powerset
almost trivial.


foldM

Recall foldl's type:
> foldl :: (a -> b -> a) -> a -> [b] -> a

foldM's type:
> foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

Here is a simple function that sums a list using fold
> foldl (\acc x -> acc + x) 0 [2,8,3,1]
<>14

What if we now want to sum numbers but if any of those numbers are > 9, then
the whole summation fails?

> binSmalls :: Int -> Int -> Maybe Int
> binSmalls acc x
>     | x > 9     = Nothing
>     | otherwise = Just (acc + x)

The binary function now returns a Maybe monad, so:

> foldM binSmalls 0 [2,8,3,1]
<>14
> foldM binSmalls 0 [2,11,3,1]
<>Nothing

Folding with a Writer would also be useful for such circumstances.


Making a Safe RPN Calculator

The RPN calculator we made in chapter 10 worked as long as the input made
sense, otherwise it would crash. Let's change it to use Maybes instead.

Recall:
> import Data.List
>
> solveRPN :: String -> Double
> solveRPN = head . foldl foldingFunction [] . words
>
> foldingFunction :: [Double] -> String -> [Double]
> foldingFunction (x:y:ys) "*" = (y * x):ys
> foldingFunction (x:y:ys) "+" = (y + x):ys
> foldingFunction (x:y:ys) "-" = (y - x):ys
> foldingFunction xs numberString = read numberString:xs

The new foldingFunction is capable of graceful failure, so it's type will
change to:
> foldingFunction :: [Double] -> String -> Maybe [Double]

We'll also use reads function: it returns a list with a single element if
successful. If it fails, it returns an empty list. Apart from the value,
reads also returns a list of the string it didn't consume. We will say that
it must always consume the full input to work. We'll wrap it in another
function for convenience:

> readMaybe :: (Read a) => String -> Maybe a
> readMaybe st = case reads st of [(x, "")] -> Just x
>                                 _         -> Nothing

Tesing out readMaybe:
> readMaybe "1" :: Maybe Int
<>Just 1
> readMaybe "GOTO HELL" :: Maybe Int
<>Nothing

It works, so let's update foldingFunction
> foldingFunction :: [Double] -> String -> Maybe [Double]
> foldingFunction (x:y:ys) "*" = return $ (y * x):ys
> foldingFunction (x:y:ys) "+" = return $ (y + x):ys
> foldingFunction (x:y:ys) "-" = return $ (y - x):ys
> foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

The first 3 cases are the same as the old, except putting the result in a
minimal context.

> foldingFunction [3,2] "*"
<>Just [6.0]
> foldingFunction [3,2] "-"
<>Just [-1.0]
> foldingFunction [] "*"
<>Nothing
> foldingFunction [] "1"
<>Just [1.0]
> foldingFunction [] "1 wawawawa"
<>Nothing

Now it's time for the new solveRPN:

> import Data.List

> solveRPN :: String -> Maybe Double
> solveRPN st = do
>     [result] <- foldM foldingFunction [] (words st)
>     return result

We turn a string into a list of strings with words. We then use foldM with
foldingFunction and an empty list as the stack as pass the [String] to it.
The result of foldM is a Maybe [Double], so we pattern match on the single
element and return it. If the pattern match fails, we get Nothing returned.

> solveRPN "1 2 * 4 +"
<>Just 6.0
> solveRPN "1 2 * 4 + 5 *"
<>Just 30.0
> solveRPN "1 2 * 4"
<>Nothing
> solveRPN "1 8 wharglbllargh"
<>Nothing

The first failure is due to a stack that's not a single element. The second
fails as readMaybe returns a Nothing.


Composing Monadic Functions

when talking about monad laws in Chapter 13, we learned that <=< is just
like composition but for monads. E.g.:

> let f = (+1) . (*100)
> f 4
<>401
> g :: (Monad m, Num a) => a -> m a
> g = (\x -> return (x+1)) <=< (\x -> return (x*100))
> Just 4 >>= g
<>Just 401
(The only way I could get the code to work, due to type ambiguity, was to
define g's type...)

If you have a bunch of functions in a list you can compose them all into
one big function by using id as the starting accumulator and the (.)
function as the binary function:

> let f = foldr (.) id [(+1),(*100),(+2)]
> f 1
<>301

f takes a number, adds 2 to it, multiplies by 100 then finally adds 1.

We can compose monadic functions in the same way, but instead of (.) and id
we use <=< and return

Recall in3, working out which positions a Knight can reach in 3 moves:
> in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

To check if a particular position can be reached in 3 moves:
> canReachIn3 :: KnightPos -> KnightPos -> Bool
> canReachIn3 start end = end `elem` in3 start

Using monadic function composition, we can create a function like in3
except instead of generating all the positions a knight can have after 3
moves, we can have an arbitrary number:

> import Data.List
> import Control.Monad
>
> inMany :: Int -> KnightPos -> [KnightPos]
> inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

First we use replicate to make x copies of moveKnight in a list. Then we use foldr with <=< to compose the monadic functions and use return as the
accumulator.

We can also change canReachIn3 to be more general:

> canReachIn :: Int -> KnightPos -> KnightPos -> Bool
> canReachIn x start end = end `elem` inMany x start
>
> canReachIn 5 (1,1) (8,8)
<>False
> canReachIn 6 (1,1) (8,8)
<>True


Making Monads

We have a list [3,5,9] but we want to express the probability of each
value occurring. We want to add a probability context to each value,
something like this:
> [(3,0.5),(5,0.25),(9,0.25)]

Floating point numbers to represent probabilities isn't a good idea, so
let's use Rational.

> import Data.Ratio
> 1%4
<> 1 % 4
> 1%2 + 1%2
<>1 % 1
> 1%3 + 5%4
<>19 % 12

So our list with probability context will look like:
> [(3,1%2),(5,1%4),(9,1%4)]

Wrapping this in a newtype gives a constructor and a way of getting the
value back out again.

> import Data.Ratio
>
> newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

list is a functor, and we added extra stuff to a list, so this should be a
functor too. When we map a function over list, we apply it to each element.
In our case we want to do the same, except leave the probabilities as they
are.

> instance Functor Prob where
>     fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

We unwrap from the newtype, apply f to each value (leave probability as it is) and wrap the result back up.

> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])
<>Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}

The probabilities should always add up to one.

But, is this a Monad? It looks like it should be. Let's think about return.

For lists, return takes a single value and puts it in a singleton list.
Here our minimal context is a value plus a probability. If the monadic
value returned must always be presented, then it makes sense that the
probability of the value being returned is 1.

What about >>=? Let's make use of the fact that for monads
m >>= f == join (fmap f m). This means to make >>= all we have to think
about is how to join, or flatten, probability lists.

Let's make up some numbers and see how it should play out: There is a 25%
chance that one of 'a' or 'b' will occur. 'a' and 'b' are equally likely to
occur. There is a 75% chance that one of 'c' or 'd' will occur. Again, 'c'
and 'd' are equally likely to occur.

If we work out what the absolute probabilities are for each of 'a', 'b',
'c', and 'd' we'd get: a:1%4*1%2==1%8; b:1%8; c:3%8; d:3%8.

As a probability list:
> situation :: Prob (Prob Char)
> situation = Prob
>     [(Prob [('a',1%2),('b',1%2)], 1%4)
>     ,(Prob [('c',1%2),('d',1%2)], 3%4)
>     ]

So to flatten the probabilites, we have to multiply the external
probability by the internal one for each value in the list.

> flatten :: Prob (Prob a) -> Prob a
> flatten (Prob xs) = Prob $ concat $ map multAll xs
>     where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

multAll takes a tuple of a probability list and a probability and multiplies the probability with each of the probabilities in the probability list.

We map multAll over each of the nested probability lists, concatenating the
result into one list and then wrapping it up.

Now we can write our Monad instance.

> instance Monad Prob where
>     return x = Prob [(x,1%1)]
>     m >>= f  = flatten (fmap f m)
>     fail _   = Prob []

The instance is simple because we've done the hard work with fmap and
flatten. Also if a pattern match fails, we get an empty list.

It's important to check that the Monad laws hold. (These are the first 2):
> (return $ Prob [('a',1%2),('b',1%2)]) >>= id
<>Prob {getProb = [('a',1 % 2),('b',1 % 2)]}
> id Prob [('a',1%2),('b',1%2)]
<>Prob {getProb = [('a',1 % 2),('b',1 % 2)]}
> Prob [('a',1%2),('b',1%2)] >>= return
<>Prob {getProb = [('a',1 % 2),('b',1 % 2)]}

It's pointing out the obvious, really, that this isn't a rigorous proof.

The third law, f <=< (g <=< h) == (f <=< g) <=< h, should hold because it
holds for the list monad and that forms the basis of the probability monad
and also because multiplication is associative.

Now that we have a monad, let's do things with it: We have two normal coins
and one loaded coin. The loaded coin has tails 9%10 and heads 1%10. If we
throw all three coins at once, what's the odds of them all being tails?

> import Data.List (all)
>
> data Coin = Heads | Tails deriving (Show, Eq)
>
> coin :: Prob Coin
> coin  = Prob [(Heads,1%2),(Tails,1%2)]
>
> loadedCoin :: Prob Coin
> loadedCoin  = Prob [(Heads,1%10),(Tails,9%10)]
>
> flipThree :: Prob Bool
> flipThree  = do
>     a <- coin
>     b <- coin
>     c <- loadedCoin
>     return (all (==Tails) [a,b,c])

Now to use it:

> getProb flipThree
<>[(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40)
<>,(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]

The False outcomes are not joined together as our function doesn't know
how to do it. This shouldn't be difficult to add.
