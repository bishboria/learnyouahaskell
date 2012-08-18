ZIPPERS

Because data structures are immutable in Haskell, changing a value actually
involves returning a completely new data structure. Also, if you want to
change a value that was close to the one you just changed the tree must be
traversed again.

Here we'll take a data structure and add to it a Zipper to focus on part of
a data structure in a way that makes changing elements easy and moving
around efficient.


Taking a Walk

> data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
>
> freeTree :: Tree Char
> freeTree =
>     Node 'P'
>         (Node 'O'
>             (Node 'L'
>                 (Node 'N' Empty Empty)
>                 (Node 'T' Empty Empty)
>             )
>             (Node 'Y'
>                 (Node 'S' Empty Empty)
>                 (Node 'A' Empty Empty)
>             )
>         )
>         (Node 'L'
>             (Node 'W'
>                 (Node 'C' Empty Empty)
>                 (Node 'R' Empty Empty)
>             )
>             (Node 'A'
>                 (Node 'A' Empty Empty)
>                 (Node 'C' Empty Empty)
>             )
>         )

How would we go about changing 'W' to a 'P'? We could pattern match: First
go to the right subtree, then the left subtree:

> changeToP :: Tree Char -> Tree Char
> changeToP (Node x l (Node y (Node _ m n) r)) =
>     Node x l (Node y (Node 'P' m n) r)

It's quite confusing and has to be read carefully to see what's going on.

Is there a better way of doing this? We could make our function take a tree
along with a list of directions, made up of L (left) or R (right). We change
the element arrived at given the directions:

> data Direction = L | R deriving (Show)
> type Directions = [Direction]
>
> changeToP :: Directions -> Tree Char -> Tree Char
> changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
> changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
> changeToP []     (Node _ l r) = Node 'P' l r

If the first element in the directions is L, then we recursivley call
changeToP on the left subtree with the tail of the directions. If we get an
R, then we recursivley call on the right subtree. If we have an empty
direction list, we've reached our target and set the value to 'P'.

To avoid printing out the whole tree, we can use this:

> elemAt :: Directions -> Tree a -> a
> elemAt (L:ds) (Node _ l _) = elemAt ds l
> elemAt (R:ds) (Node _ l r) = elemAt ds r
> elemAt []     (Node x _ _) = x

It takes a list of directions and a tree and gives back the elment at that
location.

> let newTree = changeToP [R,L] freeTree
> elemAt [R,L] newTree
<>'P'

The directions to the element act as a focus, because it pinpoints one exact
subtree. This is cool, but can be quite inefficient if you want to
repeatedly change elements.
