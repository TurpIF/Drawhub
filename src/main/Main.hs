module Main where

import Codec.Picture
import Codec.Picture.Types

import Data.Maybe
import qualified Data.Vector as V

import Drawhub.Image
import Drawhub.KMeans
import Drawhub.Region

import System.Environment
import System.Exit

handleError :: Either String a -> IO a
handleError (Left msg) = putStrLn ("Error: " ++ msg) >> exitFailure
handleError (Right a) = return a

handleMaybe :: Maybe a -> IO a
handleMaybe (Just x) = return x
handleMaybe Nothing = putStrLn "Error: Nothing" >> exitFailure

vectorFromRGB :: PixelRGB8 -> V.Vector Double
vectorFromRGB (PixelRGB8 r g b) = V.fromList $ fromIntegral <$> [r, g, b]

clustering :: Int -> Image PixelRGB8 -> [Point Int] -> [[Point Int]]
clustering nbClusters img points = clusterElems <$> fromMaybe [] (kmeans' points)
    where kmeans' = kmeans distL2 (\(Point x y) -> vectorFromRGB $ pixelAt img x y) nbClusters

main :: IO ()
main = do
    args <- getArgs
    let inputPath = head args
    let outputPath = head $ tail args
    image <- fmap convertRGB8 $ readImage inputPath >>= handleError
    print (imageWidth image, imageHeight image)
    let region = Region (Point 0 0) (Point (imageWidth image `div` 2) (imageHeight image `div` 2))
    let subImage = downscale (Size 100 100) image
    let positions = [(i, j) | i <- [0..(imageWidth subImage - 1)], j <- [0..(imageHeight subImage - 1)]]
    let quantImage = quantization (clustering 8 subImage) subImage
    let dynImage = ImageRGB8 quantImage
    savePngImage outputPath dynImage
    print "Finished"
