import Data.List
import Data.Char

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
t' = tails [1,2,3]

h = "hawaii" `isPrefixOf` "hawaii joe"
h' = "haha" `isPrefixOf` "ha"
h'' = "ha" `isPrefixOf` "ha"

a = any (> 4) [1,2,3]
a' = any (== 'F') "Frank Sobotka"
a'' = any (\x -> x > 5 && x < 10) [1,4,11]

isIn :: Eq a => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)
-- isIn is the same as Data.List.IsInfixOf

-- Caesar Cipher
o = ord 'a'
c = chr 97
mo = map ord "abcdefgh"

encode :: Int -> String -> String
encode offset msg = map (chr . (+ offset) . ord) msg -- (\c -> chr $ ord c + offset)

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

-- foldl (+) 0 (replicate 1000000 1) doesn't cause a stackoverflow...
-- foldl (+) 0 (replicate 10000000 1) doesn't cause a stackoverflow...
-- foldl (+) 0 (replicate 100000000 1) causes memory allocation failure...
-- foldl' (+) 0 (replicate 100000000 1) works
