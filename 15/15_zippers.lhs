ZIPPERS

Because data structures are immutable in Haskell, changing a value actually
involves returning a completely new data structure. Also, if you want to
change a value that was close to the one you just changed the tree must be
traversed again.

Here we'll take a data structure and add to it a Zipper to focus on part of
a data structure in a way that makes changing elements easy and moving
around efficient.
