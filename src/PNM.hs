module PNM where

import Geometry
import Screen

-- | PBM structure.
data PBM a = PBM Screen [a]

class PNM a where
  pnmString :: [a] -> String

instance PNM [a] where
  -- | Conversion of data to PBM values.
  pnmString = concatMap toPixel
    where toPixel [] = "0 "
          toPixel _  = "1 "

instance PNM (Intersection f a) where
  -- | Conversion of data to PBM values.
  pnmString = concatMap toPixel
    where toPixel (Inter Nothing) = "0 "
          toPixel _               = "1 "

instance (PNM a) => Show (PBM a) where
  show (PBM screen hs) = unlines $ [header, size] ++ pbmData
    where header  = "P1"
          size    = show screen
          pbmData = slice 70 (pnmString hs)
          slice n = takeWhile (not.null) . map (take n) . iterate (drop n)
