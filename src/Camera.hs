module Camera where

import Data.Maybe
import Vector
import Geometry
import Screen
import PNM

-- | Camera. First argument is position, second one is direction.
data Camera a = Camera (Vector a) (Vector a) Screen deriving (Eq, Show)

-- | List of points the camera has to put the ray through.
targetPoints :: (Floating a, Enum a) => Camera a -> [Vector a]
targetPoints (Camera s d (Screen width height)) = fmap (|+| middlepoint) coords
  where middlepoint = s |+| d
        left        = norm $ cross d (Vector 0 1 0)
        up          = norm $ cross d left
        step        = 2.0 / fromIntegral width
        hX          = fromIntegral height * step
        coords      = [left |* (1.0 - x) |+| up |* (hX / 2.0 - y) | y <- [0.0, step..hX - step]
                                                                  , x <- [0.0, step..2.0 - step]]

-- | List of rays the camera is casting.
rays :: (Floating a, Enum a) => Camera a -> [Line a]
rays camera@(Camera s _ _) = fmap (\t -> Line s (t |-| s)) (targetPoints camera)

-- | List of maybe hited points on geometries.
hits :: (Intersect f, Floating a, Enum a, Ord a, Eq (f a)) => Camera a -> [f a] -> [Intersection f a]
hits camera gs = fmap (`nearestHit` gs) (rays camera)

-- | Render scene basing on geometries and the camera.
render :: (Intersect f, Floating a, Enum a, Ord a, Show a, Eq (f a)) => Camera a -> [f a] -> String
render camera@(Camera _ _ screen) gs = show $ PNM screen (hits camera gs)

-- Phong model
-- light_color = ambient_color
--     |+| diffuse_color |* (maximum (dot geometry_normal normalized_light) 0)
--     |+| spectral_color |* (exp (maximum (dot normalized_reflected_light normalized_ray_from_camera) 0) shinyness)
