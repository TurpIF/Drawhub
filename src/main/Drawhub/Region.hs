module Drawhub.Region (
    Size ( .. ),

    Point ( .. ),
    pointX,
    pointY,
    add,

    Region ( .. ),
    regionTopLeft,
    regionBottomRight,
    regionWidth,
    regionHeight,
    regionSize,
    isInclude
) where

data Size = Size Int Int deriving Show

data Point = Point Int Int deriving Show

data Region = Region Point Point deriving Show

pointX :: Point -> Int
pointX (Point x _) = x

pointY :: Point -> Int
pointY (Point _ y) = y

add :: Point -> Point -> Point
(Point x0 y0) `add` (Point x1 y1) = Point (x0 + x1) (y0 + y1)

regionTopLeft :: Region -> Point
regionTopLeft (Region p _) = p

regionBottomRight :: Region -> Point
regionBottomRight (Region _ p) = p

regionDelta :: (Point -> Int) -> Region -> Int
regionDelta f region = f (regionBottomRight region) - f (regionTopLeft region)

regionWidth :: Region -> Int
regionWidth = regionDelta pointX

regionHeight :: Region -> Int
regionHeight = regionDelta pointY

regionSize :: Region -> Size
regionSize region = Size (regionWidth region) (regionHeight region)

-- check if second region is totally in first region
isInclude :: Region -> Region -> Bool
isInclude limit sub
    | pointX (regionTopLeft limit) > pointX (regionTopLeft sub) = False
    | pointY (regionTopLeft limit) > pointY (regionTopLeft sub) = False
    | pointX (regionBottomRight limit) < pointX (regionBottomRight sub) = False
    | pointY (regionBottomRight limit) < pointY (regionBottomRight sub) = False
    | otherwise = True
