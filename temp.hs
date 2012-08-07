import Data.Monoid
lengthCompare    :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels  = length . filter (`elem` "aeiou")

lengthCompare "zen" "anna"
-- LT
lengthCompare "zen" "ana"
-- LT
lengthCompare "zen" "ann"
-- GT

-- The Ordering monoid allows us to easily compare things by many different
-- criteria and put those criteria in an order themselves, ranging from
-- most important to least.
