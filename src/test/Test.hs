module Main where

import Test.QuickCheck
import System.Exit (exitFailure)

main :: IO ()
main = do
    putStrLn "This test always fails!"
    exitFailure
