module PNM where

import qualified Data.Text as T
import Text.Wrap
import Geometry
import Screen

-- | PBM structure.
data PBM a = PBM Screen [a]

class PNM a where
  pnmPixel :: a -> T.Text
  pnmString :: [a] -> T.Text

instance PNM (Intersection f a) where
  -- | Conversion of data to PBM values.
  pnmString components = wrapText (WrapSettings False False) 70 pnmData
    where pnmData = T.intercalate (T.pack " ") pixels
          pixels  = fmap pnmPixel components

  pnmPixel (Inter Nothing) = T.pack "0"
  pnmPixel _               = T.pack "1"

instance (PNM a) => Show (PBM a) where
  show (PBM screen hs) = unlines $ [header, size] ++ [pbmData]
    where header  = "P1"
          size    = show screen
          pbmData = T.unpack $ pnmString hs
