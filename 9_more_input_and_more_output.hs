-- Input Redirection

import Control.Monad
import Data.Char

main = forever $ do
    l <- getLine
    putStrLn $ map toUpper l
-- run ./9_capslocker < 9_haiku.txt to see the output

-- Getting Strings from Input Streams
-- The lazy version!

import Data.Char

main = do
    contents <- getContents
    putStr $ map toUpper contents
-- run ./9_capslocker_stream

import Data.Char

main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly = unlines . fliter (\line -> length line < 10) . lines
-- run ./9_capslocker_stream_short


-- Transforming Input

-- taking string input, transforming with a function and outputting
-- the result is so common they made a function called interact

main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines
-- run ./9_shortlinesonly_interact


-- create a function that takes input and tells the user if the line is
-- a palindrome

main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes =
    unlines .
    map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") .
    lines

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs
-- run ./9_isPalindrome

-- READING AND WRITING FILES

-- reading/writing from/to stdin/stdout is just like reading/writing to files

import System.IO

main = do
    handle <- openFile "9_girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
-- run ./9_girlfriend

-- or we could do the same thing but automatically handling opening
-- and closing the file if we use withFile

:t withFile
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

import System.IO

main = do
    withFile "9_girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
-- run ./9_girlfriend_withFile

-- It's Bracket Time

:t bracket
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

-- first param acquires a resource.
-- second param releases that resource (even if an exception raised).
-- third param does something with the resource

-- So can implement withFile using bracket easily
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile name mode f = bracket (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)


-- Grab the Handles!
