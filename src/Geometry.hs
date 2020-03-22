module Geometry where

import Data.Maybe
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

data Intersection f a = Inter { getInter :: Maybe (f a, Vector a) }

class Intersect f where
  intersection  :: (Floating a, Eq a, Ord a) => Line a ->  f a  -> Intersection f a
  intersections :: (Floating a, Eq a, Ord a) => Line a -> [f a] -> [(f a, Vector a)]

  intersections ray gs = concatMap toA traces
    where traces               = fmap (intersection ray) gs
          toA (Inter (Just a)) = [a]
          toA (Inter Nothing)  = []

instance Intersect Plane where
  -- | Intersection of ray and plane.
  intersection ray plane
            | s == 0    = Inter Nothing
            | t > 0     = Inter $ Just (plane, a |+| b |* t)
            | otherwise = Inter Nothing
            where (Line a b)  = norm ray
                  (Plane c d) = norm plane
                  s           = dot b d
                  t           = dot (c |-| a) d / s

instance Intersect Triangle where
  -- | Intersection of ray and triangle.
  intersection ray triangle@(Triangle a b c)
            | _a == 0   = Inter Nothing
            | u < 0     = Inter Nothing
            | u > 1     = Inter Nothing
            | v < 0     = Inter Nothing
            | u + v > 1 = Inter Nothing
            | t > 0     = Inter $ Just (triangle, s |+| d |* t)
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
