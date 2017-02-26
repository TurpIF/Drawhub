module Drawhub.KMeans (
    Distance,
    distL1,
    distL2,
    distLInf,

    FeatureSelection,

    Cluster,

    kmeans,
    makeCluster,
    clusterElems
) where

import Data.List
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Vector (Vector)
import qualified Data.Vector as V

type Distance a b = Vector a -> Vector a -> b

type FeatureSelection a b = a -> Vector b

type Centroid b = Vector b

newtype Cluster a = Cluster [a]

instance Functor Cluster where
    fmap f (Cluster e) = Cluster (fmap f e)

instance Eq a => Eq (Cluster a) where
    (==) (Cluster l) (Cluster r) = l == r

instance Show a => Show (Cluster a) where
    show (Cluster e) = "Cluster: " ++ show e

makeCluster :: [a] -> Cluster a
makeCluster = Cluster

clusterElems :: Cluster a -> [a]
clusterElems (Cluster xs) = xs

clusterCentroid :: Fractional b => FeatureSelection a b -> Cluster a -> Maybe (Centroid b)
clusterCentroid _ (Cluster []) = Nothing
clusterCentroid selection (Cluster xs) = Just $ V.map (/ fromIntegral (length features)) sum
    where
        sum = foldr1 (V.zipWith (+)) features
        features = selection <$> xs

nearest :: (Foldable f, Ord b) => (a -> a -> b) -> a -> f a -> Maybe a
nearest distance from ts
  | null ts = Nothing
  | otherwise = Just $ minimumBy order ts
    where order = comparing (distance from)

assignCentroid :: (Eq b, Real c) => Distance b c -> FeatureSelection a b -> [a] -> [Centroid b] -> [Cluster a]
assignCentroid distance selection features centroids = Cluster . centroidNearests <$> centroids
    where
        centroidNearests centroid = filter (isNearest centroid) features
        isNearest centroid feature = centroidNearest feature == Just centroid
        centroidNearest feature = nearest distance (selection feature) centroids

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = chunksOf' size xs
    where
        size = floor $ fromIntegral (length xs) / fromIntegral n
        chunksOf' _ [] = []
        chunksOf' n xs = take n xs : chunksOf' n (drop n xs)

kmeans :: (Eq a, Eq b, Fractional b, Real c) => Distance b c -> FeatureSelection a b -> Int -> [a] -> Maybe [Cluster a]
kmeans distance selection nbCentroids features = converge (==) $ iterate step init
    where
        init = Cluster <$> chunksOf nbCentroids features
        step clusters = assignCentroid distance selection features <$> catMaybes $ clusterCentroid selection <$> clusters

iterateM :: Monad m => (a -> m a) -> m a -> [m a]
iterateM f u = u : iterateM f (u >>= f)

converge :: (a -> a -> Bool) -> [a] -> Maybe a
converge _ [] = Nothing
converge p (x:ys@(y:_))
    | p x y = Just y
    | otherwise = converge p ys

-- L1 norm (manhattan)
distL1 :: Num a => Distance a a
distL1 a b = V.sum $ V.zipWith unit a b
    where unit x y = abs (x - y)

-- L2 norm (euclidean)
distL2 :: (Real a, Floating b) => Distance a b
distL2 a b = sqrt . realToFrac . V.sum $ V.zipWith unit a b
    where unit x y = (x - y) ^ 2

-- Linf norm (Chebyshev)
distLInf :: (Ord a, Num a) => Distance a a
distLInf a b = V.maximum $ V.zipWith unit a b
    where unit x y = abs (x - y)
