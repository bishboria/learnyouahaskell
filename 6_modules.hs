import Data.List

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

-- import Data.List (nub, sort)
-- import Data.List hiding (nub)
-- import qualified Data.Map
-- import qualified Data.Map as M
