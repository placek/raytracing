module Camera where

import Vector
import Geometry

-- | Camera. First argument is position, second one is direction.
data Camera a = Camera (Vector a) (Vector a) deriving (Eq, Show)

-- | Screen. Screen resolution information.
data Screen a = Screen a a

-- | The size of the screen in pixels.
size :: (Num a) => Screen a -> a
size (Screen w h) = w * h

instance (Show a) => Show (Screen a) where
  show (Screen w h) = (show w) ++ "x" ++ (show h)

-- | List of points the camera has to put the ray through.
targetPoints :: (Floating a, Enum a) => Camera a -> Screen a -> [Vector a]
targetPoints (Camera s d) (Screen width height) = fmap (|+| middlepoint) coords
  where middlepoint = s |+| d
        left        = norm $ cross d (Vector 0 1 0)
        up          = norm $ cross d left
        step        = 2.0 / width
        hX          = height * step
        coords      = [left |* (1.0 - x) |+| up |* (hX / 2.0 - y) | x <- [0.0, step..2.0 - step], y <- [0.0, step..hX - step]]

-- | List of rays the camera is casting.
rays :: (Floating a, Enum a) => Camera a -> Screen a -> [Line a]
rays camera@(Camera s _) screen = fmap (\t -> Line s (t |-| s)) (targetPoints camera screen)
