{-# LANGUAGE FlexibleInstances #-}
module PNM where

import qualified Data.Text as T
import Text.Wrap
import Geometry
import Vector
import Screen

grayScale :: (Fractional a, Ord a) => Vector a -> a
grayScale vector = r + g + b
  where v = cutoff vector
        r = 0.3  * _x v
        g = 0.59 * _y v
        b = 0.11 * _z v

-- | PNM structure.
data PNM a = PNM { pnmScreen :: Screen
                 , pnmData   :: [a]
                 }

-- | Conversion of PNM data to string.
pnmString :: (PNMPixel a) => PNM a -> T.Text
pnmString pnm = wrapText (WrapSettings False False) 70 result
  where result  = T.intercalate (T.pack " ") pixels
        pixels  = fmap pnmPixel (pnmData pnm)

class PNMPixel a where
  pnmPixel :: a -> T.Text

instance PNMPixel Int where
  pnmPixel a = T.pack . show $ min 0xFF a

instance (RealFrac a, Ord a) => PNMPixel (Maybe (Vector a)) where
  pnmPixel (Just v) = T.pack . show . floor . (*255) . grayScale $ v
  pnmPixel Nothing  = T.pack "32"

instance PNMPixel (Intersection f a) where
  pnmPixel (Inter Nothing) = T.pack "50"
  pnmPixel _               = T.pack "200"

instance (PNMPixel a) => Show (PNM a) where
  show pnm = unlines [header, size, maxPx, body]
    where header = "P2"
          size   = show . pnmScreen $ pnm
          maxPx  = show 0xFF
          body   = T.unpack . pnmString $ pnm
