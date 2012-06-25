main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b

-- return wraps a value in an IO action
-- <- takes an IO action, performs it, returns the value & binds to a name
-- they are kind of opposites
