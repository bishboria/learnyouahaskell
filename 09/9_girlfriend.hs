import System.IO

main = do
    handle <- openFile "9_girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
