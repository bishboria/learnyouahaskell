-- usage: import qualified Geometry.Volume as Volume
module Geometry.Volume
( cube
, cuboid
, sphere
) where

cube :: Float -> Float
cube side = cuboid side side side

cuboid :: Float -> Float -> Float -> Float
cuboid a b c = a * b * c

sphere :: Float -> Float
sphere radius = (4.0 / 3.0) * pi * (radius ^ 3)
