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
