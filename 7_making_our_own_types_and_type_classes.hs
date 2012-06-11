module Shapes
( Point(..)
, Shape(..)
, area
, nudge
, baseCircle
, baseRect
) where

-- data Bool' = False | True

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) =
	(abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
	Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

nudgedCircle = nudge (Circle (Point 34 34) 10) 5 10

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

nudgedRectangle = nudge (baseRect 40 100) 60 23
concentricCircles = map (baseCircle) [4,5,6,6]

-- Record Syntax

-- without record syntax... :|
data Person = Person String String Int Float String String deriving (Show)

firstName' :: Person -> String
firstName' (Person firstname _ _ _ _ _) = firstname

lastName' :: Person -> String
lastName' (Person _ lastname _ _ _ _) = lastname

age' :: Person -> Int
age' (Person _ _ age _ _ _) = age

height' :: Person -> Float
height' (Person _ _ _ height _ _) = height

phoneNumber' :: Person -> String
phoneNumber' (Person _ _ _ _ number _) = number

flavour' :: Person -> String
flavour' (Person _ _ _ _ _ flavour) = flavour

guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
guy'sFirstName = firstName' guy
guy'sHeight = height' guy
guy'sFlavour = flavour' guy

-- with record syntax :)
data Person2 = Person2 { firstName :: String
		       , lastName :: String
		       , age :: Int
		       , height :: Float
		       , phoneNumber :: String
		       , flavour :: String } deriving (Show)

frank = Person2 "Frank" "J" 12 1.12 "1234123" "arst"
frank' = Person2 { firstName="Frank", lastName="J"
                 , age=12, height=1.12
		 , phoneNumber="1234123", flavour="arst" }

data Car = Car { company :: String
               , model :: String
	       , year :: Int
	       } deriving (Show)

-- Can change the order of the fields
-- Can't do this without record syntax
myBaby = Car {model="Mustang", company="Ford", year=1967}

-- use record syntax with lots of params that aren't obvious
-- e.g. data Vector = Vector Int Int Int is resonably obvious


-- Type Parameters

-- data Maybe a = Nothing | Just a -- Maybe is a type constructor
maybeInt = Just 3 :: Maybe Int
maybeFloat = Just 3 :: Maybe Float

-- A type is concrete if it doesn't take any parameters at all, like
-- Int or Bool, or if it takes type parameters and they're all filled
-- up, like Maybe Char.

data IntMaybe = INothing | IJust Int
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | ShJust Shape
-- type params are better as you don't get ^^ that.

tellCar :: Car -> String
tellCar (Car {company=c, model=m, year=y}) =
	"This " ++ c ++ " " ++ m ++ " was made in " ++ show y
stang = Car {company="Ford", model="Mustang", year=1967}
-- parameterising Car value constructor i.e. Car a b c = Car {...
-- is pointless as tellCar becomes more complicated and we'd end
-- up using Car String String Int most of the time anyway.

-- bad practice to put type constraints in data declarations
-- e.g. toList :: Ord k => Map k a -> [(k, a)] doesn't care about
-- the order of the keys, so no need for (Ord k)

-- No type constraint
data Vector a = Vector a a a deriving (Show)
-- But you still have to for the functions
vplus :: Num a => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: Num a => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: Num a => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

v1 = Vector 3 5 8 `vplus` Vector 9 2 8
v2 = Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
v3 = Vector 3 9 7 `vmult` 10
v4 = Vector 4 9 5 `dotProd` Vector 9.0 2.0 4.0
v5 = Vector 2 9 3 `vmult` (Vector 4 9 5 `dotProd` Vector 9 2 4)

-- Derived Instances
-- Equating People
data Person3 = Person3 { firstName3 :: String
		       , lastName3 :: String
		       , age3 :: Int
		       } deriving (Eq, Show, Read)

mikeD = Person3 {firstName3="Michael", lastName3="Diamond", age3=43}
adRock = Person3 {firstName3="Adam", lastName3="Horovitz", age3=41}
mca = Person3 {firstName3="Adam", lastName3="Yauch", age3=44}

mca'adRock = mca == adRock
mikeD'adRock = mikeD == adRock
mikeD'mikeD = mikeD == mikeD
mikeD'newPerson = mikeD == Person3 {firstName3="Michael", lastName3="Diamond", age3=43}

beastieBoys = [mca, adRock, mikeD]
isMikeDInBeastieBoys = mikeD `elem` beastieBoys

showMikeD = show mikeD
showMikeD' = "mikeD is: " ++ show mikeD

mysteryDude = "Person3 { firstName3=\"Michael\"" ++
	              ", lastName3=\"Diamond\"" ++
		      ", age3=43}"
readMysteryDude = read mysteryDude :: Person3

readJust3 = read "Just 3" :: Maybe Int

-- data Bool = False | True deriving (Ord)
trueCompFalse = True `compare` False
trueGtFalse = True > False
trueLtFalse = True < False
-- instances of Ord are ordered in the manner they are defined.
nothingLtJust100 = Nothing < Just 100
nothingGtJustMinus49999 = Nothing > Just (-49999)
just3cmpJust2 = Just 3 `compare` Just 2
just100GtJust50 = Just 100 > Just 50
-- can't do Just (*3) > Just (*2). (*3),(*2) aren't instances of Ord


-- Any Day of the Week
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
	   deriving (Eq, Ord, Show, Read, Bounded, Enum)
