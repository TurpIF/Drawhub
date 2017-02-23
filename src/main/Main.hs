module Main where

import Codec.Picture
import Codec.Picture.Types

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

main :: IO ()
main = do
    args <- getArgs
    let inputPath = head args
    let outputPath = head $ tail args
    image <- fmap convertRGB8 $ readImage inputPath >>= handleError
    print (imageWidth image, imageHeight image)
    let region = Region (Point 0 0) (Point (imageWidth image `div` 2) (imageHeight image `div` 2))
    let subImage = downscale (Size 70 70) image
    let dynImage = ImageRGB8 subImage
    let positions = [(i, j) | i <- [0..(imageWidth subImage - 1)], j <- [0..(imageHeight subImage - 1)]]
    clusters <- handleMaybe $ kmeans distL2 (\(x, y) -> vectorFromRGB $ pixelAt subImage x y) 5 positions
    sequence_ $ print <$> clusters
    savePngImage outputPath dynImage
    print "Finished"
