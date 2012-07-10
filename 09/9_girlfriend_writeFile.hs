import System.IO
import Data.Char

main = do
    contents <- readFile "9_girlfriend.txt"
    writeFile "9_girlfriendcaps.txt" (map toUpper contents)
