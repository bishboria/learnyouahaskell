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


Focusing On Lists

Zippers can be used with pretty much any data structure, so it's no surprise
that they work with sublists of lists.

Lists are simpler than trees: we don't need to remember left or right,
there's only one way to go deeper in a list. It seems like all we need to
remember is the previous element.

Because a single breadcrumb is just a single element, we don't really need
a new data type:

> type ListZipper a = ([a],[a])
>
> goForward :: ListZipper a -> ListZipper a
> goForward (x:xs, bs) = (xs, x:bs)
>
> goBack :: ListZipper a -> ListZipper a
> goBack (xs, b:bs) = (b:xs, bs)

When we go forward, we focus on the tail of the current list and leave the
head element as a breadcrumb. When moving back, we put the latest breadcrumb
back at the beginning of the list.

> xs = [1,2,3,4]
> goForward (xs, [])
<>([2,3,4],[1])
> goForward ([2,3,4], [1])
<>([3,4],[2,1])
> goForward ([3,4],[2,1])
<>([4],[3,2,1])
> goBack ([4],[3,2,1])
<>([3,4],[2,1])

You can see that the breadcrumbs are nothing more than the reversed part of
the list.

If you are making a text editor, you could use a list of strings to reprsent
lines of text that are currently opened. You could then use a zipper so that
you know which line is currently in focus. Using a zipper would also make it
easy to add new lines of text anywhere and delete existing ones.


A Very Simple Filesystem

To demonstrate how zippers work, let's use trees to represent a very simple
filesystem. Then make a zipper for that filesystem.

We'll assume the usual hierarchical File/Folder structure and create some
types

> type Name = String
> type Data = String
> data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

Here's an example filesystem:

> sk :: FSItem
> myDisk =
>     Folder "root"
>         [ File "goat_yelling_like_man.wmv" "baaaaaa"
>         , File "pope_time.avi" "god bless"
>         , Folder "pics"
>             [ File "ape_throwing_up.jpg" "bleargh"
>             , File "watermelon_smash.gif" "smash!!"
>             , File "skull_man(scary).bmp" "Yikes!"
>             ]
>         , File "dijon_poupon.doc" "best mustard"
>         , Folder "programs"
>             [ File "fartwizard.exe" "10gotofart"
>             , File "owl_bandit.dmg" "mov eax, h00t"
>             , File "not_a_virus.exe" "really not a virus"
>             , Folder "source code"
>                 [ File "best_hs_prog.hs" "main = print (fix error)"
>                 , File "random.hs" "main = print 4"
>                 ]
>             ]
>         ]

Now we have a filesystem, all we need is a zipper to do all the things we
need to do with a bunch of files and folders.

For the filesystem, a breadcrumb should be like a folder because if we're
focusing on a file we can't move deeper into the filesystem so it doesn't
make sense to leave a breadcrumb that says we just came from a file.

If we are focusing on the folder root and then we focus on the file
"dijon_poupon.doc", the breadcrumb should contain the name of its parent
folder along with the items that come before and after the file on which
we're focusing. So all we need is a Name and two lists of items. By keeping
two lists, we are able to put back the current item of focus back to the
exact position it was in when we move back up.

> data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
> type FSZipper = (FSItem, [FSCrumb])

Going back up the hierarchy is quite easy. Take the current breadcrumb and
assemble a new focus from the current focus and the breadcrumb

> fsUp :: FSZipper -> FSZipper
> fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

Because the zipper knew the name of the parent folder as well as what came
before and after the item, moving up was easy.

How about going further down the filesystem?

> import Data.List (break)
>
> fsTo :: Name -> FSZipper -> FSZipper
> fsTo name (Folder folderName items, bs) =
>     let (ls, item:rs) = break (nameIs name) items
>     in  (item, FSCrumb folderName ls rs:bs)
>
> nameIs :: Name -> FSItem -> Bool
> nameIs name (Folder folderName _) = name == folderName
> nameIs name (File fileName _)     = name == fileName

We only look at the current folder for the given name.

break takes a predicate and a list and returns a pair of lists. The first
list holds all the elements for which the predicate is false. The second list
contains as its head the first element for which the predicate was true,
followed by all the other elements.

If the name wasn't found, item:rs will produce an error due to failed
pattern matching.

Let's start at the root and walk to file "skul_man(scary).bmp":

> newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skul_man(scary).bmp"
> fst newFocus
<>File "skull_man(scary).bmp" "Yikes!"

Moving up to its neighboring file "watermelon_smash.gif"

> newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"
> fst newFocus2
<>File "watermelon_smash.gif" "smash!!"


Manipulating A Filesystem

We can navigate the filesystem now. Here's a function that renames the currently
focused item

> fsRename :: Name -> FSZipper -> FSZipper
> fsRename newName (Folder name items, bs) = (Folder newName items, bs)
> fsRename newName (File name data, bs)    = (File newName data, bd)

Let's rename folder "pics" to "cspi"

> newFocus = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp

And a function that makes a new item in the current folder:

> fsNewFile :: FSItem -> FSZipper -> FSZipper
> fsNewFile item (Folder folderName items, bs) =
>     (Folder folderName (item: items), bs)
>
> (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp

Remember that this pattern matches a folder only, so will error if focused on a
file.

And because the structures are immutable, when we modify the filesystem we are
actually getting a new one back. So we have access to the old filesystem too.
Versioning for free.

There are lots of cases where this filesystem would fail, such as trying to
focus on a file or folder that doesn't exist... So a lot more code needs to be
written for it.


Watch Your Step

So far we haven't dealt with errors if we walk around too far in the data
structures. It seems that if we move left, right, or up we may get a successful
value or we may get an error. This sounds like a Maybe

> goLeft :: Zipper a -> Maybe (Zipper a)
> goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
> goLeft (Empty, _) = Nothing
>
> goRight :: Zipper a -> Maybe (Zipper a)
> goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
> goRight (Empty, _) = Nothing
>
> goLeft (Empty, [])
<>Nothing
> goLeft (Node 'A' Empty Empty, [])
<>Just (Empty, [LeftCrumb 'A' Empty])

How about going up?

> goUp :: Zipper a -> Maybe (Zipper a)
> goUp (t, LeftCrumb x r:bs)  = Just (Node x t r, bs)
> goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
> goUp (_, []) = Nothing

Now we can't chain functions together using -: and will instead use >>=

> coolTree = Node 1 Empty (Node 3 Empty Empty)
> return (coolTree, []) >>= goRight
<>Just (Node 3 Empty Empty,[RightCrumb 1 Empty])
> return (coolTree, []) >>= goRight >>= goRight
<>Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])
> return (coolTree, []) >>= goRight >>= goRight >>= goRight
<>Nothing
