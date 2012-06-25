main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- recursive function is ok because main is an IO action
-- and the do block inside the else wraps two IO actions into one

-- return in haskell does not exit a function. It wraps a pure value in an
-- IO action. Monad stuff again
