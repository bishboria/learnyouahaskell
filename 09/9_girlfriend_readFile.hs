import System.IO

main = do
    contents <- readFile "9_girlfriend.txt"
    putStr contents
