module Vector where

import Data.Monoid
import Data.Group

-- | Vector. Three dimensional structure with @_x@, @_y@ and @_z@ as components.
data Vector a = Vector { _x :: a, _y :: a, _z :: a } deriving (Eq, Show)

instance Functor Vector where
  fmap f (Vector a b c) = Vector (f a) (f b) (f c)

instance (Num a) => Semigroup (Vector a) where
  (Vector a b c) <> (Vector d e f) = Vector (a + d) (b + e) (c + f)

instance (Num a) => Monoid (Vector a) where
  mempty = Vector 0 0 0

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
a |-| b = a <> invert b

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
dot :: (Num a) => Vector a -> Vector a -> a
dot (Vector a b c) (Vector d e f) = a * d + b * e + c * f

-- | Cross product.
cross :: (Num a) => Vector a -> Vector a -> Vector a
cross (Vector a b c) (Vector d e f) = Vector (b * f - c * e) (c * d - a * f) (a * e - b * d)

-- | Magnitude. The length of the vector.
magnitude :: (Floating a) => Vector a -> a
magnitude a = sqrt $ dot a a

-- | Distance.
distance :: (Floating a) => Vector a -> Vector a -> a
distance a b = magnitude $ a |-| b

-- | Reflection.
reflect :: (Floating a) => Vector a -> Vector a -> Vector a
reflect normal vector = 2 * dot v n *| n |-| v
  where v = norm vector
        n = norm normal

-- | Cutoff components at 1.0.
cutoff :: (Fractional a, Ord a) => Vector a -> Vector a
cutoff = fmap (min 1.0)

class Norm a where
  norm :: a -> a

instance (Floating a) => Norm (Vector a) where
  -- | Normalize vector.
  -- All vectors components are divided by its magnitude.
  norm v = v |/ magnitude v
