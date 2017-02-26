module Drawhub.Github (
    githubFitImage
) where

import Codec.Picture
import Codec.Picture.Types

import Data.Maybe

import qualified Data.Vector as V

import Drawhub.Image
import Drawhub.KMeans
import Drawhub.Region

githubNbShades :: Int
githubNbShades = 5

githubMaxWidth :: Int
githubMaxWidth = 52

githubMaxHeight :: Int
githubMaxHeight = 7

vectorFromRGB :: PixelRGB8 -> V.Vector Double
vectorFromRGB (PixelRGB8 r g b) = V.fromList $ fromIntegral <$> [r, g, b]

clustering :: Int -> Image PixelRGB8 -> [Point Int] -> [[Point Int]]
clustering nbClusters img points = clusterElems <$> fromMaybe [] (kmeans' points)
    where kmeans' = kmeans distL2 (\(Point x y) -> vectorFromRGB $ pixelAt img x y) nbClusters

githubResize :: Image PixelRGB8 -> Image PixelRGB8
githubResize img = downscale (scaleFixedHeight githubMaxHeight $ imageSize img) img

-- 5 shades of greens
githubShade :: Image PixelRGB8 -> Image PixelRGB8
githubShade img = quantization (clustering githubNbShades img) img

githubFitImage :: Image PixelRGB8 -> Image PixelRGB8
githubFitImage = githubShade . githubResize

