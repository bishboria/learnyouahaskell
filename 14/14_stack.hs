import Control.Monad.State

type Stack = [Int]

pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x, xs)

push' :: Int -> Stack -> ((), Stack)
push' a xs = ((), a:xs)

stackManip' :: Stack -> (Int, Stack)
stackManip' stack = 
    let ((), newStack1) = push' 3 stack
        (a, newStack2)  = pop' newStack1
    in pop' newStack2

-- stackManip' [5,8,2,1]
-- (5,[8,2,1])


-- Now as the State Monad
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

-- runState stackManip [5,8,2,1]

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

-- runState stackStuff [9,0,2,1,0]
-- ((),[8,3,0,2,1,0])

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()

-- runState moreStack [8,3,2,1]
-- ((),[3,2,1])
-- runState moreStack [100,3,2,1]
-- ((),[8,3,2,1])
-- runState moreStack [100,5,3,2,1]
-- ((),[5,3,2,1])
