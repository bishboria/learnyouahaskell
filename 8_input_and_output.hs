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
