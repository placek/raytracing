module Camera where

import Data.Maybe
import Vector
import Geometry
import Screen
import PNM

-- | Camera. First argument is position, second one is direction.
data Camera a = Camera (Vector a) (Vector a) Screen

-- | Light. First argument is position, second one is direction.
data Light a = Light (Vector a) (Vector a) (Vector a) (Vector a)

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
render :: (Floating a, RealFrac a, Enum a, Ord a, Show a) => Camera a -> Light a -> [Triangle a] -> String
render camera@(Camera _ _ screen) light gs = show $ PNM screen (fmap (lightColor camera light) (hits camera gs))

-- | Phong model
lightColor :: (Floating a, Ord a) => Camera a -> Light a -> Intersection Triangle a -> Maybe (Vector a)
lightColor _              _                    (Inter Nothing)          = Nothing
lightColor (Camera c _ _) (Light source a d s) (Inter (Just (g, _, p))) = Just . cutoff $ ambient |+| diffuse |+| spectral
  where ambient   = a
        diffuse   = d |* (max (dot gNormal light) 0)
        spectral  = s |* ((max (dot reflected camera) 0) ** shinyness)
        gNormal   = normal g
        light     = norm $ p |-| source
        reflected = reflect gNormal light
        camera    = norm $ p |-| c
        shinyness = 0.4
