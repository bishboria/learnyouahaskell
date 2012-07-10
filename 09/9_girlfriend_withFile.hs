import System.IO

main = do
    withFile "9_girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
