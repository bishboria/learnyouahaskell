doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
			 then x
			 else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2)

conanO'Brien = "It's a-me, Conan O'Brien!"
lostNumbers = [4,8,15,16,23,42]

concatenateLists = [1,2,3,4] ++ [9,10,11,12] -- [a] ++ [a]
concatenateStrings = "hello" ++ " " ++ "world"
concatenateStrings' = ['w','0'] ++ ['0','t']

consStrings = 'A':" SMALL CAT" -- a:[a]
consNumbers = 5:[1,2,3,4,5]
consNumbers' = 5:1:2:3:4:5:[]

indexString = "Steve Buscemi" !! 6
indexList = [9.4,33.2,96.2,11.2] !! 1
listInList = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4]]
indexList' = listInList !! 1

compareList = [3,2,1] > [2,1,0] -- determined element by element
compareList' = [3,4,2] < [3,4,3]

headList = head [5,4,3,2,1]
tailList = tail [5,4,3,2,1]
lastList = last [5,4,3,2,1]
initList = init [5,4,3,2,1]

startList = [5,4,3,2,1]
middleOfList = init (tail startList)
middleOfList' = tail (init startList)

lengthList = length [5,4,3,2,1]

isNull = null [1,2,3]

reverseList = reverse [5,4,3,2,1]

takeFirst3 = take 3 [5,4,3,2,1]
takeFirst100 = take 100 [5,4,3,2,1]
takeNone = take 0 [5,4,3,2,1]

max = maximum [1,9,2,3,4]
min = minimum [8,4,2,1,5,6]

sumList = sum [5,2,1,6,3,2,5,7]
productList = product [6,2,1,2]

inList = elem 4 [3,4,5,6]
inList' = 4 `elem` [3,4,5,6]

rangeNumber = [1..20]
rangeCharacter = ['k'..'z']
rangeStep = [2,4..20]
rangeDecreasing = [20,19..1]

rangeSteppedInfiniteTake = take 10 [13,26..]

cycledList = take 10 (cycle [1,2,3])
repeatedElement = take 10 (repeat 5)
replicatedElement = replicate 3 10 -- 3 `replicate` 10

dodgyFloatRange = [0.1, 0.3 .. 1]

comprehensionMultiplesTwo = [x*2 | x <- [1..10]]
comprehensionMultiplesTwoGreaterThanTwelve = [x*2 | x <- [1..10], x*2 >= 12]
comprehensionMod = [x | x <- [50.100], x `mod` 7 == 3]
