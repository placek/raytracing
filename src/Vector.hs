module Vector where

import Data.Monoid
import Data.Group

data Vector a = Vector a a a deriving (Eq, Show)

instance Functor Vector where
  fmap f (Vector a b c) = Vector (f a) (f b) (f c)

instance (Num a) => Semigroup (Vector a) where
  (Vector a b c) <> (Vector d e f) = Vector (a + d) (b + e) (c + f)

instance (Num a) => Monoid (Vector a) where
  mempty = Vector (fromInteger 0) (fromInteger 0) (fromInteger 0)

instance (Num a) => Group (Vector a) where
  invert (Vector a b c) = Vector (-a) (-b) (-c)

dot :: (Num a) => (Vector a) -> (Vector a) -> a
dot (Vector a b c) (Vector d e f) = a * d + b * e + c * f

cross :: (Num a) => (Vector a) -> (Vector a) -> (Vector a)
cross (Vector a b c) (Vector d e f) = Vector (b * f - c * e) (c * d - a * f) (a * e - b * d)

magnitude :: (Floating a) => (Vector a) -> a
magnitude a = sqrt $ dot a a

norm :: (Floating a) => (Vector a) -> (Vector a)
norm v@(Vector a b c) = Vector (a / mag) (b / mag) (c / mag)
            where mag = magnitude v
