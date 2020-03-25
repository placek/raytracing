module Geometry where

import Data.List
import Vector

-- | Line. First argument is some point on the line. Second argument is a direction vector of the line.
data Line a = Line (Vector a) (Vector a) deriving (Eq, Show)

-- | Plane. First argument is some point on the plane. Second argument is the vector orthogonal to the plane.
data Plane a = Plane (Vector a) (Vector a) deriving (Eq, Show)

-- | Triangle.
data Triangle a = Triangle (Vector a) (Vector a) (Vector a) deriving (Eq, Show)

instance (Floating a) => Norm (Line a) where
  -- | Normalize line.
  -- Directional vector is normalized..
  norm (Line s d) = Line s (norm d)

instance (Floating a) => Norm (Plane a) where
  -- | Normalize plane.
  -- Normal vector is normalized..
  norm (Plane s d) = Plane s (norm d)

normal (Triangle a b c) = cross edge1 edge2
  where edge1 = b |-| a
        edge2 = c |-| b

newtype Intersection f a = Inter { getInter :: Maybe (f a, a, Vector a) } deriving Eq

instance (Ord a, Eq (f a)) => Ord (Intersection f a) where
  compare _ (Inter Nothing) = LT
  compare (Inter Nothing) _ = GT
  compare (Inter (Just (_, a, _))) (Inter (Just (_, b, _))) = compare a b

class Intersect f where
  setIntersection :: (Floating a) => f a -> Vector a -> Vector a -> Intersection f a
  intersection    :: (Floating a, Eq a, Ord a) => Line a ->  f a  -> Intersection f a
  nearestHit      :: (Floating a, Eq a, Ord a, Eq (f a)) => Line a -> [f a] -> Intersection f a

  setIntersection geometry source target = Inter $ Just (geometry, distance source target, target)
  nearestHit ray gs = minimum (fmap (intersection ray) gs)

instance Intersect Plane where
  -- | Intersection of ray and plane.
  intersection ray@(Line source _) plane
            | s == 0    = Inter Nothing
            | t > 0     = setIntersection plane source (a |+| b |* t)
            | otherwise = Inter Nothing
            where (Line a b)  = norm ray
                  (Plane c d) = norm plane
                  s           = dot b d
                  t           = dot (c |-| a) d / s

instance Intersect Triangle where
  -- | Intersection of ray and triangle.
  intersection ray@(Line source _) triangle@(Triangle a b c)
            | _a == 0   = Inter Nothing
            | u < 0     = Inter Nothing
            | u > 1     = Inter Nothing
            | v < 0     = Inter Nothing
            | u + v > 1 = Inter Nothing
            | t > 0     = setIntersection triangle source (s |+| d |* t)
            | otherwise = Inter Nothing
            where (Line s d) = norm ray
                  edge1      = b |-| a
                  edge2      = c |-| a
                  h          = cross d edge2
                  _a         = dot h edge1
                  u          = dot (s |-| a) h / _a
                  q          = cross (s |-| a) edge1
                  v          = dot d q / _a
                  t          = dot edge2 q / _a
