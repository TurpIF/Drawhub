{-# LANGUAGE Rank2Types #-}

module Main where

import System.IO
import System.Environment
import System.Exit

import Codec.Picture
import Codec.Picture.Types

import Data.Either
import Data.Maybe
import Data.Word

data Point = Point Int Int deriving Show
data Region = Region Point Point deriving Show
data Size = Size Int Int deriving Show

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

imageRegion :: Pixel p => Image p -> Region
imageRegion img = Region (Point 0 0) (Point (imageWidth img) (imageHeight img))

viewSubImage :: forall a . (Pixel a) => Region -> Image a -> Either String (Image a)
viewSubImage slice src
    | isInclude (imageRegion src) slice = Right $ generateImage viewPixels (regionWidth slice) (regionHeight slice)
    | otherwise = Left $ "Given region " ++ show slice ++ " is not included in given image of dimension" ++ show (imageRegion src)
    where
        viewPixels x y = pixelAt src (pointX vp) (pointY vp)
            where vp = regionTopLeft slice `add` Point x y

imageSum :: Image PixelRGB8 -> (Int, Int, Int)
imageSum = pixelFold acc (0, 0, 0)
    where acc (ar, ag, ab) _ _ (PixelRGB8 r g b) = (ar + fromIntegral r, ag + fromIntegral g, ab + fromIntegral b)

imageMean :: Image PixelRGB8 -> PixelRGB8
imageMean img = toPixel $ imageSum img
    where
        imgSize = imageWidth img * imageHeight img
        toPixel (r, g, b) = PixelRGB8 (to8 r) (to8 g) (to8 b)
            where to8 c = (fromIntegral $ ceiling (fromIntegral c / fromIntegral imgSize)) :: Word8

downscale :: Size -> Image PixelRGB8 -> Image PixelRGB8
downscale (Size w h) src = generateImage f w h
    where
        widthRatio = fromIntegral (imageWidth src) / fromIntegral w
        heightRatio = fromIntegral (imageHeight src) / fromIntegral h
        f x y = let (Right px) = imageMean <$> viewSubImage region src in px
            where
                x0 = fromIntegral $ floor (fromIntegral x * widthRatio)
                y0 = fromIntegral $ floor (fromIntegral y * heightRatio)
                x1 = fromIntegral $ ceiling ((fromIntegral x + 1) * widthRatio)
                y1 = fromIntegral $ ceiling ((fromIntegral y + 1) * heightRatio)
                region = Region (Point x0 y0) (Point x1 y1)

handleError :: Either String a -> IO a
handleError (Left msg) = putStrLn ("Error: " ++ msg) >> exitFailure
handleError (Right a) = return a

main :: IO ()
main = do
    args <- getArgs
    let inputPath = head args
    let outputPath = head $ tail args
    image <- fmap convertRGB8 $ readImage inputPath >>= handleError
    print (imageWidth image, imageHeight image)
    let region = Region (Point 0 0) (Point (imageWidth image `div` 2) (imageHeight image `div` 2))
    let subImage = downscale (Size 7 7) image
    let dynImage = ImageRGB8 subImage
    savePngImage outputPath dynImage
    print "Finished"
