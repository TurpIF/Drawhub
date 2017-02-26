module Drawhub.Github (
    GithubImage ( .. ),

    fitImage,
    imageToCalendar
) where

import Codec.Picture
import Codec.Picture.Types

import Control.Monad

import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as V

import Drawhub.Image
import Drawhub.KMeans
import Drawhub.Region

data Activity = A0 | A1 | A2 | A3 | A4
newtype Calendar = Calendar [Activity]
newtype GithubImage = GithubImage { getRgbImage :: (Image PixelRGB8) }

githubNbShades :: Int
githubNbShades = 5

githubMaxWidth :: Int
githubMaxWidth = 52

githubMaxHeight :: Int
githubMaxHeight = 7

activityRgb :: Activity -> PixelRGB8
activityRgb A0 = PixelRGB8 238 238 238
activityRgb A1 = PixelRGB8 214 230 133
activityRgb A2 = PixelRGB8 140 198 101
activityRgb A3 = PixelRGB8 68 163 64
activityRgb A4 = PixelRGB8 30 104 35

rgbToActivity :: PixelRGB8 -> Maybe Activity
rgbToActivity rgb
    | activityRgb A0 == rgb = Just A0
    | activityRgb A1 == rgb = Just A1
    | activityRgb A2 == rgb = Just A2
    | activityRgb A3 == rgb = Just A3
    | activityRgb A4 == rgb = Just A4
    | otherwise = Nothing

vectorFromRGB :: PixelRGB8 -> V.Vector Double
vectorFromRGB (PixelRGB8 r g b) = V.fromList $ fromIntegral <$> [r, g, b]

rgbSaturation :: PixelRGB8 -> Double
rgbSaturation (PixelRGB8 r g b) = if max == 0 then 0 else min / max where
    min = fromIntegral $ minimum [r, g, b]
    max = fromIntegral $ maximum [r, g, b]

clustering :: Int -> Image PixelRGB8 -> [Point Int] -> [[Point Int]]
clustering nbClusters img points = clusterElems <$> fromMaybe [] (kmeans' points) where
    kmeans' = kmeans distL2 (\(Point x y) -> vectorFromRGB $ pixelAt img x y) nbClusters

-- TODO only one resize
githubResize :: Image PixelRGB8 -> Image PixelRGB8
githubResize img = if imageWidth resized > githubMaxWidth
    then downscaling (scaleFixedWidth githubMaxWidth) resized
    else resized where
    downscaling f img = downscale (f $ imageSize img) img
    resized = downscaling (scaleFixedHeight githubMaxHeight) img

-- 5 shades of greens
githubShade :: Image PixelRGB8 -> Image PixelRGB8
githubShade img = pixelMap getShade quantizedImage where
    quantizedImage = quantization (clustering githubNbShades img) img
    orderedShades = sort . nubOrd $ rgbSaturation <$> traversePixels quantizedImage
    getShade px = activityRgb . activityOf . fromJust $ elemIndex (rgbSaturation px) orderedShades where
        activityOf 0 = A0
        activityOf 1 = A1
        activityOf 2 = A2
        activityOf 3 = A3
        activityOf 4 = A4
        activityOf _ = error "no more activities"

fitImage :: Image PixelRGB8 -> GithubImage
fitImage img = GithubImage $ (githubShade . githubResize) img

imageToCalendar :: GithubImage -> Calendar
imageToCalendar img = Calendar $ fromJust . rgbToActivity <$> traversePixels (getRgbImage img)

nubOrd :: Ord a => [a] -> [a]
nubOrd = go Set.empty where
    go _ [] = []
    go s (x:xs)
        | x `Set.member` s = go s xs
        | otherwise = x : go (Set.insert x s) xs
