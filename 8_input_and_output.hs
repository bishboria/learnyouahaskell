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
