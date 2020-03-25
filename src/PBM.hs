module PBM where

import Geometry
import Screen

-- | PBM structure.
data PBM a = PBM Screen [a]

class PbmImage a where
  toPbmString :: [a] -> String

instance PbmImage [a] where
  -- | Conversion of data to PBM values.
  toPbmString = concatMap toPixel
    where toPixel [] = "0 "
          toPixel _  = "1 "

instance PbmImage (Intersection f a) where
  -- | Conversion of data to PBM values.
  toPbmString = concatMap toPixel
    where toPixel (Inter Nothing) = "0 "
          toPixel _               = "1 "

instance (PbmImage a) => Show (PBM a) where
  show (PBM screen hs) = unlines $ [header, size] ++ pbmData
    where header  = "P1"
          size    = show screen
          pbmData = slice 70 (toPbmString hs)
          slice n = takeWhile (not.null) . map (take n) . iterate (drop n)
