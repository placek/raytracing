module Geometry where

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

class Intersect f where
  intersection  :: (Floating a, Eq a, Ord a) => f a ->  Line a  -> Maybe (Vector a)
  intersections :: (Floating a, Eq a, Ord a) => f a -> [Line a] -> [Maybe (Vector a)]

  intersections geometry = fmap (intersection geometry)

instance Intersect Plane where
-- | Intersection of ray and plane.
  intersection plane ray
            | s == 0    = Nothing
            | t > 0     = Just $ a |+| b |* t
            | otherwise = Nothing
            where (Line a b)  = norm ray
                  (Plane c d) = norm plane
                  s           = dot b d
                  t           = dot (c |-| a) d / s

instance Intersect Triangle where
-- | Intersection of ray and triangle.
  intersection (Triangle a b c) ray
            | _a == 0   = Nothing
            | u < 0     = Nothing
            | u > 1     = Nothing
            | v < 0     = Nothing
            | u + v > 1 = Nothing
            | t > 0     = Just $ s |+| d |* t
            | otherwise = Nothing
            where (Line s d) = norm ray
                  edge1      = b |-| a
                  edge2      = c |-| a
                  h          = cross d edge2
                  _a         = dot h edge1
                  u          = dot (s |-| a) h / _a
                  q          = cross (s |-| a) edge1
                  v          = dot d q / _a
                  t          = dot edge2 q / _a
