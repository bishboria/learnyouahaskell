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


A Trail of Breadcrumbs

Would it help our structure if we left a "breadcrumb" trail as we traversed
the structure? A list of directions will be our breadcrumbs, and it will be
in reverse order.

> type Breadcrumbs = [Direction]

The following function takes a tuple containing a tree and breadcrumbs and
returns a tuple containing the left subtree and the direction taken at the
top of new breadcrumbs.

> goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
> goLeft (Node _ l _, bs) = (l, L:bs)

And also to go right:

> goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
> goRight (Node _ _ r, bs) = (r, R:bs)

Let's take our freeTree and go right then left.

> goLeft $ goRight (freeTree, [])
<>(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])

To make walking along the tree clearer we could use the -: function from
Chapter 13:

> x -: f = f x

So we can rewrite the above to be:

> (freeTree, []) -: goRight -: goLeft
<>(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])


Going Back Up

What if we want to go back up the tree? The breadcrumbs only tell us where
we went, there is no information about the parent at all.

In general a single breadcrumb should contain all the data needed to
reconstruct the parent node. We also don't want to duplicate any
information: If we are editing a value at the bottom of the tree then we
need to be sure that there are no copies of it elsewhere that will be out of
date.

The newest breadcrumbs will contain all the information about the directions
we took and the values we ignored by not going a certain direction:

> data Crumb a =
>     LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

Now instead of just L, we have LeftCrumb which contains the element in the
node we moved from and the right subtree we ignored. RightCrumb contains the
element from the node we moved from and the left subtree we ignored.

We are now storing all the data required to rebuild the tree that we walked
through.

> type Breadcrumbs a = [Crumb a]

We also need to modify goLeft and goRight to store information about the
paths that we didn't take, instead of ignoring them as before:

> goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
> goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

So now if going left, we make the left subtree the current tree we are
focussed on and create a new LeftCrumb that remembers the ignored Node
element and right subtree and puts that as the head of the list. It also
assumes the current tree under focus isn't empty...

> goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
> goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

Going back up is just a case of pattern matching the breadcrumb and putting
the values back in the correct place and keeping the remaining breadcrumbs:

> goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
> goUp (t, LeftCrumb  x r:bs) = (Node x t r, bs)
> goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

This function will cause an error if we try to move up too far. Later on
we'll change it to use Maybe to represent possible failure.

Now that we have a pair consisting a Tree and Breadcrumbs, we are able to
traverse the tree to focus on a subtree and then reconstruct the original
when we are done.

A pair that contains a part of a data structure and its surroundings is
called a zipper. We can create a type synonym for this:

> type Zipper a = (Tree a, Breadcrumbs a)


Manipulating Trees Under Focus

We can move up and down, now let's modify the element in the root of a
subtree on which the zipper is focusing:

> modify :: (a -> a) -> Zipper a -> Zipper a
> modify f (Node x l r, bs) = (Node (f x) l r, bs)
> modify f (Empty, bs) = (Empty, bs)

We modify the value with a function if we are on a Node, we leave it as is
if Empty.

> newFocus = modify (\_ -> 'P') (goRight (goLeft (freeTree, [])))

Or with -:

> newFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')

We can then move up and replace another element with 'X'

> newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

Moving up is easy because of the Breadcrumbs. If we have navigated to an
Empty subtree, then we would like to be able to replace it with a nonempty
one

> attach :: Tree a -> Zipper a -> Zipper a
> attach t (_, bs) = (t, bs)

Not only can we extend trees this way, but we can replace existing subtrees.

> farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
> newFocus' = farLeft -: attach (Node 'Z' Empty Empty)

newFocus' is focussed on the newly attached tree and the rest of the tree is
stored inverted in the breadcrumbs.


Going Straight To The Top

Making a function that walks all the way to the top, regardless of where
you are currently focusing is quite easy:

> topMost :: Zipper a -> Zipper a
> topMost (t, []) = (t, [])
> topMost z = topMost (goUp z)
