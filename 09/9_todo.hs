import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ =
    putStrLn $ "The " ++ command ++ " command doesn't exist"

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments"

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                [0..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStrLn "The view command takes one argument"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    replaceFileContents fileName newTodoItems
remove _ = putStrLn "The remove command takes exactly two arguments"

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        number = read numberString
        todo = todoTasks !! number
        newTodoItems = unlines $ [todo] ++ delete todo todoTasks
    replaceFileContents fileName newTodoItems
bump _ = putStrLn "The bump command takes exactly two arguments"

replaceFileContents :: FilePath -> String -> IO ()
replaceFileContents fileName contents =
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle contents
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)
-- In the book removeFile "todo.txt" & renameFile... are still hardcoded!
-- Tut. Tut. Tut.
