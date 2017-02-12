{-# LANGUAGE Rank2Types #-}

module Drawhub.Image (
    viewSubImage,
    downscale
) where

import Codec.Picture
import Codec.Picture.Types

import Data.Either
import Data.Maybe
import Data.Word

import Drawhub.Region

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
