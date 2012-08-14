type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = 
    let ((), newStack1) = push 3 stack
        (a, newStack2)  = pop newStack1
    in pop newStack2

-- stackManip [5,8,2,1]
-- (5,[8,2,1])
