module Camera where

import Data.Maybe
import Vector
import Geometry

-- | Screen. Screen resolution information.
data Screen a = Screen a a deriving (Eq)

-- | Camera. First argument is position, second one is direction.
data Camera a = Camera (Vector a) (Vector a) (Screen a) deriving (Eq, Show)

-- | The size of the screen in pixels.
size :: (Num a) => Screen a -> a
size (Screen w h) = w * h

instance (Show a) => Show (Screen a) where
  show (Screen w h) = show w ++ "x" ++ show h

-- | List of points the camera has to put the ray through.
targetPoints :: (Floating a, Enum a) => Camera a -> [Vector a]
targetPoints (Camera s d (Screen width height)) = fmap (|+| middlepoint) coords
  where middlepoint = s |+| d
        left        = norm $ cross d (Vector 0 1 0)
        up          = norm $ cross d left
        step        = 2.0 / width
        hX          = height * step
        coords      = [left |* (1.0 - x) |+| up |* (hX / 2.0 - y) | y <- [0.0, step..hX - step]
                                                                  , x <- [0.0, step..2.0 - step]]

-- | List of rays the camera is casting.
rays :: (Floating a, Enum a) => Camera a -> [Line a]
rays camera@(Camera s _ _) = fmap (\t -> Line s (t |-| s)) (targetPoints camera)

-- | List of maybe hited points on geometries.
hits :: (Intersect f, Floating a, Enum a, Ord a) => Camera a -> [f a] -> [[(f a, Vector a)]]
hits camera gs = fmap (\r -> intersections r gs) (rays camera)

-- | PBM structure.
data PBM a b = PBM a a [[b]]

-- | Conversion of data to PBM values.
toPbmString :: [[a]] -> String
toPbmString = concatMap toPixel
  where toPixel [] = "0 "
        toPixel _  = "1 "

instance (Show a) => Show (PBM a b) where
  show (PBM w h hs) = unlines $ [header, size] ++ pbmData
    where header  = "P1"
          size    = takeWhile (/= '.') (show w) ++ " " ++ takeWhile (/= '.') (show h)
          pbmData = slice 70 (toPbmString hs)
          slice n = takeWhile (not.null) . map (take n) . iterate (drop n)

-- | Render scene basing on geometries and the camera.
render :: (Intersect f, Floating a, Enum a, Ord a, Show a) => Camera a -> [f a] -> String
render camera@(Camera _ _ (Screen w h)) gs = show $ PBM w h (hits camera gs)
