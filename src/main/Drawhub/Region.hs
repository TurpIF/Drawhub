module Drawhub.Region (
    Size ( .. ),

    Point ( .. ),
    pointX,
    pointY,
    pointBimap,
    add,

    Region ( .. ),
    regionTopLeft,
    regionBottomRight,
    regionWidth,
    regionHeight,
    regionSize,
    isInclude
) where

data Size a = Size a a deriving Show

data Point a = Point a a deriving Show

data Region a = Region (Point a) (Point a) deriving Show

instance Functor Size where
    fmap f (Size w h) = Size (f w) (f h)

instance Functor Point where
    fmap f (Point x y) = Point (f x) (f y)

instance Eq a => Eq (Point a) where
    (==) (Point x0 y0) (Point x1 y1) = x0 == x1 && y0 == y1

instance Functor Region where
    fmap f (Region p0 p1) = Region (fmap f p0) (fmap f p1)

pointX :: Point a -> a
pointX (Point x _) = x

pointY :: Point a -> a
pointY (Point _ y) = y

pointBimap :: (a -> b) -> (a -> b) -> Point a -> Point b
pointBimap fx fy (Point x y) = Point (fx x) (fy y)

add :: Num a => Point a -> Point a -> Point a
(Point x0 y0) `add` (Point x1 y1) = Point (x0 + x1) (y0 + y1)

regionTopLeft :: Region a -> Point a
regionTopLeft (Region p _) = p

regionBottomRight :: Region a -> Point a
regionBottomRight (Region _ p) = p

regionDelta :: Num b => (Point a -> b) -> Region a -> b
regionDelta f region = f (regionBottomRight region) - f (regionTopLeft region)

regionWidth :: Num a => Region a -> a
regionWidth = regionDelta pointX

regionHeight :: Num a => Region a -> a
regionHeight = regionDelta pointY

regionSize :: Num a => Region a -> Size a
regionSize region = Size (regionWidth region) (regionHeight region)

-- check if second region is totally in first region
isInclude :: Ord a => Region a -> Region a -> Bool
isInclude limit sub
    | pointX (regionTopLeft limit) > pointX (regionTopLeft sub) = False
    | pointY (regionTopLeft limit) > pointY (regionTopLeft sub) = False
    | pointX (regionBottomRight limit) < pointX (regionBottomRight sub) = False
    | pointY (regionBottomRight limit) < pointY (regionBottomRight sub) = False
    | otherwise = True
