module Monoids where

-- to define a shape you define a function that says if a point is in the shape or not
newtype Shape coord = Shape {
    isInShape :: coord -> Bool
}

type Coord2d = (Double, Double)
type Shape2d = Shape Coord2d
type Radius = Double

outside :: Shape coord -> Shape coord
outside s = Shape (not . isInShape s)

disk :: Coord2d -> Radius -> Shape2d
disk center radius = Shape $ \coord -> distance center coord <= radius

intersect :: Shape coord -> Shape coord -> Shape coord
intersect s1 s2 = Shape $ \coord -> isInShape s1 coord && isInShape s2 coord

intersectAll :: Foldable f => f (Shape coord) -> Shape coord
intersectAll shapes = Shape $ \coord -> all (`isInShape` coord) shapes

allSpace :: Shape coord
allSpace = Shape (const True)

-- distance
distance :: Coord2d -> Coord2d -> Double
distance (x1 , y1) (x2 , y2) = sqrt (x'**2 + y'**2)
    where
        x' = x1 - x2
        y' = y1 - y2
