{-# LANGUAGE Rank2Types           #-}

module Drawhub.Image (
    imageSize,
    viewSubImage,
    downscale,
    quantization,
    traversePixels,
    traversePosPixels
) where

import Codec.Picture
import Codec.Picture.Types

import Data.Either
import Data.Maybe
import Data.Word

import Drawhub.Region

data Color3 a = Color3 a a a deriving Show

instance Functor Color3 where
    fmap f (Color3 a b c) = Color3 (f a) (f b) (f c)

colorFromRGB :: Num a => PixelRGB8 -> Color3 a
colorFromRGB (PixelRGB8 r g b) = fromIntegral <$> Color3 r g b

colorToRGB :: Integral a => Color3 a -> PixelRGB8
colorToRGB (Color3 a b c) = PixelRGB8 (fromIntegral a) (fromIntegral b) (fromIntegral c)

colorAdd :: Num a => Color3 a -> Color3 a -> Color3 a
colorAdd (Color3 a b c) (Color3 a' b' c') = Color3 (a + a') (b + b') (c + c')

colorSum :: (Num a, Traversable t) => t (Color3 a) -> Color3 a
colorSum = foldr colorAdd $ Color3 0 0 0

colorMean :: (Fractional a, Traversable t) => t (Color3 a) -> Color3 a
colorMean cs = (/ fromIntegral (length cs)) <$> colorSum cs

imageRegion :: Pixel p => Image p -> Region Int
imageRegion img = Region (Point 0 0) (Point (imageWidth img) (imageHeight img))

viewSubImage :: forall a . (Pixel a) => Region Int -> Image a -> Either String (Image a)
viewSubImage slice src
    | isInclude (imageRegion src) slice = Right $ generateImage viewPixels (regionWidth slice) (regionHeight slice)
    | otherwise = Left $ "Given region " ++ show slice ++ " is not included in given image of dimension" ++ show (imageRegion src)
    where
    viewPixels x y = pixelAt src (pointX vp) (pointY vp)
        where vp = regionTopLeft slice `add` Point x y

imageSize :: Image a -> Size Int
imageSize img = Size (imageWidth img) (imageHeight img)

imageSum :: Num a => Image PixelRGB8 -> Color3 a
imageSum = pixelFold acc $ Color3 0 0 0
    where acc col _ _ px = colorAdd col $ fromIntegral <$> colorFromRGB px

imageMean :: Image PixelRGB8 -> PixelRGB8
imageMean img = toPixel $ imageSum img
    where
    imgSize = imageWidth img * imageHeight img
    toPixel rgb = colorToRGB $ ceiling . (/ fromIntegral imgSize) <$> rgb

downscale :: Size Int -> Image PixelRGB8 -> Image PixelRGB8
downscale (Size w h) src = generateImage generatePixel w h
    where
    generatePixel x y = assertRight $ imageMean <$> viewSubImage region src
        where
        region = fromIntegral <$> Region p0 p1
            where
            mapRatio = pointBimap (*widthRatio) (*heightRatio)
                where
                widthRatio = fromIntegral (imageWidth src) / fromIntegral w
                heightRatio = fromIntegral (imageHeight src) / fromIntegral h
            p = fromIntegral <$> Point x y
            p0 = floor <$> mapRatio p
            p1 = ceiling <$> mapRatio ((+1) <$> p)
        assertRight (Right x) = x
        assertRight (Left x) = error ("Error: " ++ show x)

type Clustering a = [a] -> [[a]]

findIn :: Eq a => a -> [[a]] -> Maybe [a]
findIn _ [] = Nothing
findIn n (x:xs)
    | n `elem` x = Just x
    | otherwise = findIn n xs

quantization :: Clustering (Point Int) -> Image PixelRGB8 -> Image PixelRGB8
quantization clustering img = generateImage generatePixel w h
    where
    w = imageWidth img
    h = imageHeight img
    positions = [Point i j | i <- [0..(w - 1)], j <- [0..(h - 1)]]
    clusters = clustering positions
    generatePixel x y = colorToRGB $ ceiling <$> (colorMean $ colorFromRGB . (\(Point x y) -> pixelAt img x y) <$> cluster)
        where cluster = fromMaybe [] $ findIn (Point x y) clusters

traversePixels :: Image PixelRGB8 -> [PixelRGB8]
traversePixels img = [pixelAt img x y | y <- [0..imageHeight img - 1], x <- [0..imageWidth img - 1]]

traversePosPixels :: Image PixelRGB8 -> [(Int, Int, PixelRGB8)]
traversePosPixels img = [(x, y, pixelAt img x y) | y <- [0..imageHeight img - 1], x <- [0..imageWidth img - 1]]
