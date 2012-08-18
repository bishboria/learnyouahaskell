data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Direction = L | R deriving (Show)
type Breadcrumbs = [Direction]

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

subtree = goLeft $ goRight (freeTree, [])
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

x -: f = f x

subtree' = (freeTree, []) -: goRight -: goLeft
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])
