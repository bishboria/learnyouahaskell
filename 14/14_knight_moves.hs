import Data.List
import Control.Monad

type KnightPos = (Int,Int)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

-- canReachIn 3 (1,1) (8,8)
-- False
-- canReachIn 4 (1,1) (8,8)
-- False
-- canReachIn 5 (1,1) (8,8)
-- False
-- canReachIn 6 (1,1) (8,8)
-- True
