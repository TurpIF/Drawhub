module  Drawhub.KMeans_UT where

import Data.Maybe
import qualified Data.Vector as V
import Drawhub.KMeans


-- chunksOf

prop_chunksOfEmpty n = chunksOf n [] == [[]]

prop_chunksOfZeroIfInf xs = len (take inf $ chunksOf 0 xs) == inf
  where inf = 2 * len xs


-- converge

prop_convergeOfFalse xs = isNothing $ converge (\x y -> False) xs

prop_convergeOfTrue [] = True
prop_convergeOfTrue xs = converge (\x y -> True) xs == Just . head . tail xs

prop_convergeOfEmpty p = isNothing $ converge p []

prop_converge = conv == converge (==) [1, 2, 1, 1, 2] == Just 1


-- iterateM

prop_iterateMIsInf f u = len (take inf $ iterateM f u) == inf
  where inf = 100

prop_iterateM = take 5 $ iterateM f (Just 0) == [Just 0, Just 1, Just 2, Nothing, Nothing]
  where f x
    | x <= 2 = Just x
    | otherwise = Nothing


-- clusterCentroid

prop_clusterCentroidOfEmptyCluster f = isNothing $ clusterCentroid f []
