doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
			 then x
			 else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

lostNumbers = [4,8,15,16,23,42]

concatenateLists = [1,2,3,4] ++ [9,10,11,12] -- [a] ++ [a]

concatenateStrings = "hello" ++ " " ++ "world"

concatenateStrings' = ['w','0'] ++ ['0','t']

consStrings = 'A':" SMALL CAT" -- a:[a]

consNumbers = 5:[1,2,3,4,5]

consNumbers' = 5:1:2:3:4:5:[]

listIndexing = "Steve Buscemi" !! 6
listIndexing' = [9.4,33.2,96.2,11.2] !! 1
