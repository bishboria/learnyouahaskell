import Control.Monad.Writer

logNumber  :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

-- runWriter multWithLog
-- (15,["Got number: 3","Got number: 5"])

multWithLog' :: Writer [String] Int
multWithLog' = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

-- runWriter multWithLog'
-- (15,["Got number: 3","Got number: 5","Gonna multiply these two"])
