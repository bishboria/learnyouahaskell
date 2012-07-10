-- from the end of chapter 6

module Geometry
( areaCube
, areaCuboid
, areaSphere
, volumeCube
, volumeCuboid
, volumeSphere
) where

areaCube :: Float -> Float
areaCube side = areaCuboid side side side

areaCuboid :: Float -> Float -> Float -> Float
areaCuboid a b c = areaRect a b * 2 + areaRect a c * 2 + areaRect c b * 2

areaRect :: Float -> Float -> Float
areaRect a b = a * b

areaSphere :: Float -> Float
areaSphere radius = 4 * pi * (radius ^ 2)

volumeCube :: Float -> Float
volumeCube side = volumeCuboid side side side

volumeCuboid :: Float -> Float -> Float -> Float
volumeCuboid a b c = areaRect a b * c

volumeSphere :: Float -> Float
volumeSphere radius = (4.0 / 3.0) * pi * (radius ^ 3)
