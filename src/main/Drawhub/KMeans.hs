module Drawhub.KMeans (
    Distance,
    distL1,
    distL2,
    distLInf,

    FeatureSelection,

    Cluster,
    clusterElems,

    kmeans
) where

import Data.List
import Data.Ord
import Data.Vector (Vector)
import qualified Data.Vector as V

type Distance a b = Vector a -> Vector a -> b

type FeatureSelection a b = a -> Vector b

type Centroid b = Vector b

newtype Cluster a = Cluster [a]

instance Functor Cluster where
    fmap f cluster = Cluster (f <$> clusterElems cluster)

instance Eq a => Eq (Cluster a) where
    (==) (Cluster l) (Cluster r) = l == r

clusterElems :: Cluster a -> [a]
clusterElems (Cluster elems) = elems

clusterCentroid :: Fractional b => FeatureSelection a b -> Cluster a -> Centroid b
clusterCentroid selection cluster = V.map (/ fromIntegral (length features)) sum
    where
        sum = foldr1 (V.zipWith (+)) features
        features = selection <$> clusterElems cluster

assignCentroid :: (Eq b, Real c) => Distance b c -> FeatureSelection a b -> [a] -> [Centroid b] -> [Cluster a]
assignCentroid distance selection features centroids = Cluster . centroidNearest <$> centroids
    where
        centroidNearest centroid = filter (isNearest centroid) features
        isNearest centroid feature = selectNearest feature == centroid
        selectNearest feature = minimumBy (comparing (distance (selection feature))) centroids

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = chunksOf' size xs
    where
        size = floor $ fromIntegral (length xs) / fromIntegral n
        chunksOf' n xs = take n xs : chunksOf' n (drop n xs)

kmeans :: (Eq a, Eq b, Fractional b, Real c) => Distance b c -> FeatureSelection a b -> [a] -> Int -> [Cluster a]
kmeans distance selection features nbCentroids = converge (==) $ iterate step init
    where
        init = Cluster <$> chunksOf nbCentroids features
        step clusters = assignCentroid distance selection features $ clusterCentroid selection <$> clusters

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y = y
    | otherwise = converge p ys

-- L1 norm (manhattan)
distL1 :: Num a => Distance a a
distL1 a b = V.sum $ V.zipWith unit a b
    where unit x y = abs (x - y)

-- L2 norm (euclidean)
distL2 :: (Real a, Floating b) => Distance a b
distL2 a b = sqrt . realToFrac . V.sum $ V.zipWith unit a b
    where unit x y = (x - y) * (x - y)

-- Linf norm (Chebyshev)
distLInf :: (Ord a, Num a) => Distance a a
distLInf a b = V.maximum $ V.zipWith unit a b
    where unit x y = abs (x - y)
