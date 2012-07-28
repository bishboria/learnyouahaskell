-- Writing an RPN function

solveRPN :: String -> Double
solveRPN  = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*"    = (y * x):ys
            foldingFunction (x:y:ys) "+"    = (y + x):ys
            foldingFunction (x:y:ys) "-"    = (y - x):ys
            foldingFunction xs numberString = read numberString:xs

-- solveRPN "10 4 3 + 2 * -"
-- solveRPN "2 3.5 +"
-- solveRPN "90 34 12 33 55 66 + * - +"
-- solveRPN "90 34 12 33 55 66 + * - + -"
-- solveRPN "90 3.8 -"
