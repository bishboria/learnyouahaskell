type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

xs = [1,2,3,4]
a = goForward (xs, [])
b = goForward ([2,3,4], [1])
c = goForward ([3,4],[2,1])
d = goBack ([4],[3,2,1])
