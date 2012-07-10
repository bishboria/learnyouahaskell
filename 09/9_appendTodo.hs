import System.IO

main = do
    todoItem <- getLine
    appendFile "9_todo.txt" (todoItem ++ "\n")
