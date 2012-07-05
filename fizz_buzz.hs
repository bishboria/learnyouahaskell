threes = cycle ["", "", "fizz"]
fives = cycle ["","","","","buzz"]
numbersString = map show [1..]
fizzbuzz = zipWith (\a b -> if b /= "" then b else a) numbersString $ zipWith (++) threes fives

fizzbuzzTo100 = take 100 fizzbuzz

oneLine = zipWith (\a b -> if b /= "" then b else a) (map show [1..]) $ zipWith (++) (cycle ["","","fizz"]) (cycle ["","","","","buzz"])
