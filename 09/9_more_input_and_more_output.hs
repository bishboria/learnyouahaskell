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

-- hGetContents, hGetLine, hPutStr, hPutStrLn, hGetChar, etc all work like
-- their -h counterparts except +h versions work with handles instead of
-- standard in/out

-- Working with strings and files is common, so we can use:
-- readFile, writeFile and appendFile

import System.IO

main = do
    contents <- readFile "9_girlfriend.txt"
    putStr contents
-- run ./9_girlfriend_readFile

import System.IO
import Data.Char

main = do
    contents <- readFile "9_girlfriend.txt"
    writeFile "9_girlfriendcaps.txt" (map toUpper contents)
-- run ./9_girlfriend_writeFile


-- To-Do Lists

-- putting appendFile to use

import System.IO

main = do
    todoItem <- getLine
    appendFile "9_todo.txt" (todoItem ++ "\n")
-- run ./9_appendTodo

-- Deleting Items

-- we can add items to the todo list, now let's delete

import System.IO
import System.Directory
import Data.List

main = do
    contents <- readFile "9_todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoItems
    hClose tempHandle
    removeFile "9_todo.txt"
    renameFile tempName "9_todo.txt"
-- run ./9_deleteTodo


-- Cleaning up

-- if the program errors after the temp file is openend then the temp file
-- does not get removed.
-- Fix this by using bracketOnError from Control.Exception
-- bracketOnError only performs the cleanup if an exception is raised.
-- bracket always performs the cleanup

import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do
    contents <- readFile "9_todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "9_todo.txt"
            renameFile tempName "9_todo.txt")
-- run ./9_deleteTodo_bracketOnError


-- Command-Line Arguments

:t getArgs
getArgs :: IO [String]
-- gets the arguments the program was run with an yields those as a list

:t getProgName
getProgName :: IO String
-- get the program name

import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName


-- More Fun with To-Do Lists

-- implementing an entire todo that takes user input and dispatches
-- based on the values matched.

import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile "9_todo.txt"
            renameFile tempName "9_todo.txt")
-- In the book removeFile "todo.txt" & renameFile... are still hardcoded!
-- Tut. Tut. Tut.
