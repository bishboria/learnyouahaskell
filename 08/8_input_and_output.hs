-- Hello, World!
-- see 8_hello_world.hs

{-
:t putStrLn
putStrLn :: String -> IO ()

:t putStrLn "hello, world"
putStrLn "hello, wolrd" :: IO ()
-}


-- Gluing IO actions together
{-
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!"
-}

-- getLine returns an IO String
-- to get at the string use <-

-- you cannot do
{-
nameTag = "Hello, my name is " ++ getLine
-}
-- ++ expects two lists. Here LHS is [Char]/String
-- RHS is IO String.

-- The last action in a do block cannot be bound to a name
-- This is because of monadic reasons.

-- Except for the last line, every line in a do block can be written with
-- a bind. But it's missed out for unimportant things like putStrLn.

{-
myLine = getLine
-}
-- all this does is give getLine a new name. myLine is still an IO action


-- Using let inside IO actions
-- see 8_upper_case_name.hs
-- the jist:
-- let for binding pure values. <- for binding results of IO actions


-- Putting it in reverse
-- see 8_reverse.hs

-- see 8_using_return_to_do_nothing.hs

-- see 8_using_return_to_do_something.hs

-- return wraps a value in an IO action
-- <- takes an IO action, performs it, returns the value & binds to a name
-- they are kind of opposites

-- it would be easier to do this though
{-
main = do
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b
-}

-- Use return when you want an IO action that does nothing.
-- Or when the result needs to be different from the last "real" action
-- peformed by the do block
-- e.g. putStrLn :: String -> IO (), but if you want an IO String, use
-- return


-- Some Useful IO Functions

{-
:t putStr
putStr :: String -> IO () -- and prints string on same line

:t putChar
putChar :: Char -> IO () -- and prints character on same line

-- can declare putStr in terms of putChar
putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs

:t print
print :: Show a => a -> IO () -- take Show instance & print on line

main = do
    print True
    print 2
    print "haha"
    print [3,4,3]

-- upon pressing enter GHCi uses print.

-- use putStrLn when you don't want quotes
-- mostly use print when output to terminal

:t when
when :: Monad m => Bool -> m () -> m ()

import Control.Monad
main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

-- when return IO action it was passed if Bool is true,
-- empty IO action otherwise

-- without when
main = do
    input <- getLine
    if (input == "SWORDFISH")
        then putStrLn input
        else return ()

:t sequence
sequence :: Monad m => [m a] -> m [a]

-- without sequence
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

-- with sequence
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

-- common pattern with sequence
sequence $ map print [1,2,3,4,5]
-- output:
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]...
-- print yields (), hence the list of ()

-- Because mapping a IO action function over a list then sequencing is so
-- common. The following were created.

:t mapM
mapM :: Monad m => (a -> m b) -> [a] -> m [b]

mapM print [1,2,3]
--1
--2
--3
--[(),(),()]

:t mapM_
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

mapM_ print [1,2,3]
--1
--2
--3

:t forever
forever :: Monad m => m a -> m b
-- takes an IO action an returns an action that performs the
-- input action forever

import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

:t forM
forM :: Monad m => [a] -> (a -> m b) -> m [b]
-- like mapM but with the parameters reversed
-- takes a list, then a function to map

import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number "
                   ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors

-- the last two lines of (\a -> do ... ) could be replaced with: getLine
-- color <- getLine unpacks the IO action
-- return color repacks it into an IO action

-- forM produces an IO action
-- colors is a [String]

-- could have done the same thing without forM but using forM makes the
-- code more readable...

-- use forM when we want to map and sequence some actions that we define
-- on the spot using do notation


-- So IO actions are performed if they fall into the main function
-- or are the result in a GHCi line. They can also yield results to show
-- what the outside world gave you.
-}
