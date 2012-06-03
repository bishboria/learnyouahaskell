import Data.List

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

-- import Data.List (nub, sort)
-- import Data.List hiding (nub)
-- import qualified Data.Map
-- import qualified Data.Map as M

w = words "hey these are the words in this sentence"
w' = words "hey these              are       the words in this sentence"
g = group [1,1,1,1,1,1,1,1,1,2,2,3,2,2,5,6,7]
g' = group ["boom","bip","bip","boom","boom"]
s = sort [5,4,3,7,2,1]
s' = sort ["boom","bip","bip","boom","boom"]

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

t = tails "party"
