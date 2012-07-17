import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 $ randomRs ('a','z') gen
    putStrLn "And now calling getStdGen again produces..."
    gen2 <- getStdGen
    putStrLn $ take 20 $ randomRs ('a','z') gen2
    putStrLn "The same random output... Look at 9_random_string_3.hs"
