-- usage: import qualified Geometry.Area as Area
module Geometry.Area
( cube
, cuboid
, sphere
) where

cube :: Float -> Float
cube side = cuboid side side side

cuboid :: Float -> Float -> Float -> Float
cuboid a b c = rect a b * 2 + rect a c * 2 + rect c b * 2

rect :: Float -> Float -> Float
rect a b = a * b

sphere :: Float -> Float
sphere radius = 4 * pi * (radius ^ 2)
