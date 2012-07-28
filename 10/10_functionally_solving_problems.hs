import Data.List -- needed for main function at bottom of file

-- Writing an RPN function

solveRPN :: String -> Double
solveRPN  = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*"    = (y * x) :ys
            foldingFunction (x:y:ys) "+"    = (y + x) :ys
            foldingFunction (x:y:ys) "-"    = (y - x) :ys
            foldingFunction (x:y:ys) "/"    = (y / x) :ys
            foldingFunction (x:y:ys) "^"    = (y ** x):ys
            foldingFunction (x:xs)   "ln"   = log x   :xs
            foldingFunction xs "sum"        = [sum xs]
            foldingFunction xs numberString = read numberString:xs

-- solveRPN "10 4 3 + 2 * -"
-- solveRPN "2 3.5 +"
-- solveRPN "90 34 12 33 55 66 + * - +"
-- solveRPN "90 34 12 33 55 66 + * - + -"
-- solveRPN "90 3.8 -"
-- solveRPN "2.7 ln"
-- solveRPN "10 10 10 10 sum 4 /"
-- solveRPN "10 10 10 10 10 sum 4 /"
-- solveRPN "10 2 ^"

-- Reminder
-- comments assume f needs to evaluate both operands before it can return
myFr f z [] = z
myFr f z (x:xs) = f x $ myFr f z xs -- aplies f x $ ... when end of list reached

myFl f z [] = z
myFl f z (x:xs) = myFl f (f z x) xs -- applies (f z x) lazily so doesn't reduce
                                    -- straight away

myFl' f z [] = z
myFl' f z (x:xs) = let z' = f z x
                   in seq z' $ myFl' f z' xs -- apply z' now and then
                                             -- return myFl' f z' xs
-- /Reminder


-- Quickest Path (Heathrow to London...)
-- Book does a terrible job explaining it!

data Section = Section { getA :: Int, getB :: Int, getC :: Int }
    deriving (Show)
-- could have used (Int,Int,Int) But then they could also be vectors...
-- declaring the type, instead, means we can't accidently add a vector to a
-- section of a road system.

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon  = [ Section 50 10 30
                    , Section 5 90 20
                    , Section 40 2 25
                    , Section 10 8 0
                    ]

data Label = A | B | C deriving (Show)
type Path  = [(Label,Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardTimeToA = timeA + a
        crossTimeToA   = timeB + b + c
        forwardTimeToB = timeB + b
        crossTimeToB   = timeA + a + c
        newPathToA = if forwardTimeToA <= crossTimeToA
                        then (A, a):pathA
                        else (C, c):(B, b):pathB
        newPathToB = if forwardTimeToB <= crossTimeToB
                        then (B, b):pathB
                        else (C, c):(A, a):pathA
    in  (newPathToA, newPathToB)
-- The paths are the wrong way round as it's faster to prepend a list...
-- If we took and returned timeA and timeB, we wouldn't need to do
--      sum (map snd pathX)
-- everytime

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then reverse bestAPath
            else reverse bestBPath

-- Getting a road system from the input

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)


main = do
    contents <- getContents
    let threes     = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path       = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathTime   = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime
