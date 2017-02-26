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

readArgs :: [String] -> Maybe (FilePath, FilePath)
readArgs (x:y:_) = Just (x, y)
readArgs _ = Nothing

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
    (inputPath, outputPath) <- handleMaybe $ readArgs args
    image <- convertRGB8 <$> (readImage inputPath >>= handleError)
    let subImage = downscale (Size 50 50) image
    let quantImage = quantization (clustering 5 subImage) subImage
    savePngImage outputPath (ImageRGB8 quantImage)
