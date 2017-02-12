module Main where

import Codec.Picture

import Drawhub.Image
import Drawhub.Region

import System.Environment
import System.Exit

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
