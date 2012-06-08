module Shapes
( Point(..)
, Shape(..)
, area
, nudge
, baseCircle
, baseRect
) where

data Bool' = False | True

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
myBaby = Car { company="Ford", model="Mustang", year=1967 }
