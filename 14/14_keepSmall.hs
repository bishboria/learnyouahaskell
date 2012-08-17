import Control.Monad.Writer

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

first = fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
second = mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
