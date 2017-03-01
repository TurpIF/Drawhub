module Drawhub.KMeans (
    Distance,
    distL1,
    distL2sq,
    distLInf,

    FeatureSelection,

    Cluster,

    kmeans,
    makeCluster,
    clusterElems
) where

import Data.List
import Data.Foldable
import Data.Function (on)
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
clusterCentroid selection (Cluster xs) = Just $ V.map (/ fromIntegral (length xs)) sum where
    sum = foldl1' (V.zipWith (+)) $ selection <$> xs

nearest :: (Foldable f, Ord b) => (a -> a -> b) -> a -> f a -> Maybe a
nearest distance from ts
  | null ts = Nothing
  | otherwise = Just $ minimumBy order ts
    where order = comparing (distance from)

assignCentroid :: (Eq b, Real c) => Distance b c -> FeatureSelection a b -> [a] -> [Centroid b] -> [Cluster a]
assignCentroid dist selection features cs = toList $ Cluster <$>
  (V.unsafeAccum (flip (:)) (V.replicate n []) $ closest <$> features) where
    closest a = (V.minIndexBy (compare `on` dist (selection a)) (V.fromList cs), a)
    n = length cs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = chunksOf' size xs
    where
        size = ceiling $ fromIntegral (length xs) / fromIntegral n
        chunksOf' _ [] = []
        chunksOf' n xs = take n xs : chunksOf' n (drop n xs)

kmeans :: (Eq a, Eq b, Fractional b, Real c) => Distance b c -> FeatureSelection a b -> Int -> [a] -> Maybe [Cluster a]
kmeans distance selection nbCentroids features = converge (==) $ iterate step init where
    init = Cluster <$> chunksOf nbCentroids features
    step clusters = assignCentroid distance selection features
        <$> catMaybes $ clusterCentroid selection <$> clusters

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
distL2sq :: Num a => Distance a a
distL2sq a b = V.sum $ V.zipWith unit a b
    where unit x y = (x - y) ^ 2

-- Linf norm (Chebyshev)
distLInf :: (Ord a, Num a) => Distance a a
distLInf a b = V.maximum $ V.zipWith unit a b
    where unit x y = abs (x - y)
