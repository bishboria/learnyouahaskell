import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args -- could also use mapM_ since we don't care about the result
    putStrLn "The program name is:"
    putStrLn progName
