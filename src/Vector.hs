module Vector where

import Data.Monoid
import Data.Group

-- | Vector. Three dimensional structure with @_x@, @_y@ and @_z@ as components.
data Vector a = Vector { _x :: a, _y :: a, _z :: a } deriving (Eq, Show)

-- | Line. First argument is some point on the line. Second argument is a direction vector of the line.
data Line a = Line (Vector a) (Vector a) deriving (Eq, Show)

-- | Plane. First argument is some point on the plane. Second argument is the vector orthogonal to the plane.
data Plane a = Plane (Vector a) (Vector a) deriving (Eq, Show)

instance Functor Vector where
  fmap f (Vector a b c) = Vector (f a) (f b) (f c)

instance (Num a) => Semigroup (Vector a) where
  (Vector a b c) <> (Vector d e f) = Vector (a + d) (b + e) (c + f)

instance (Num a) => Monoid (Vector a) where
  mempty = Vector (fromInteger 0) (fromInteger 0) (fromInteger 0)

instance (Num a) => Group (Vector a) where
  invert (Vector a b c) = Vector (-a) (-b) (-c)

infixl 4 |-|
infixl 4 |+|
infixr 5 |*
infixl 5 *|

-- | Vectors addition.
(|+|) :: (Num a) => Vector a -> Vector a -> Vector a
a |+| b = a <> b

-- | Vectors substraction.
(|-|) :: (Num a) => Vector a -> Vector a -> Vector a
a |-| b = a <> (invert b)

-- | Multiplication by scalar.
(|*) :: (Num a) => Vector a -> a -> Vector a
v |* a = fmap (*a) v

-- | Multiplication by scalar.
(*|) :: (Num a) => a -> Vector a -> Vector a
(*|) = flip (|*)

-- | Division by scalar.
(|/) :: (Fractional a) => Vector a -> a -> Vector a
v |/ a = fmap (/a) v

-- | Dot product.
dot :: (Num a) => (Vector a) -> (Vector a) -> a
dot (Vector a b c) (Vector d e f) = a * d + b * e + c * f

-- | Cross product.
cross :: (Num a) => (Vector a) -> (Vector a) -> (Vector a)
cross (Vector a b c) (Vector d e f) = Vector (b * f - c * e) (c * d - a * f) (a * e - b * d)

-- | Magnitude. The length of the vector.
magnitude :: (Floating a) => (Vector a) -> a
magnitude a = sqrt $ dot a a

class Norm a where
  norm :: a -> a

instance (Floating a) => Norm (Vector a) where
  -- | Normalize vector.
  -- All vectors components are divided by its magnitude.
  norm v = v |/ (magnitude v)

instance (Floating a) => Norm (Line a) where
  -- | Normalize line.
  -- Directional vector is normalized..
  norm (Line s d) = Line s (norm d)

instance (Floating a) => Norm (Plane a) where
  -- | Normalize plane.
  -- Normal vector is normalized..
  norm (Plane s d) = Plane s (norm d)

-- | Intersection of line and plane.
intersect :: (Floating a, Eq a) => Line a -> Plane a -> Maybe (Vector a)
intersect line plane
          | s == 0    = Nothing
          | otherwise = Just $ a |+| b |* t
          where (Line a b)  = norm line
                (Plane c d) = norm plane
                s           = dot b d
                t           = (dot (c |-| a) d) / s
