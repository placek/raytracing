module PNM where

import qualified Data.Text as T
import Text.Wrap
import Geometry
import Screen

data PNMHeader = P1 | P3 deriving Show

-- | PNM structure.
data PNM a = PNM { pnmHeader :: PNMHeader
                 , pnmScreen :: Screen
                 , pnmData :: [a]
                 }

class PNMPixel a where
  pnmPixel  :: a -> T.Text
  pnmString :: [a] -> T.Text

  -- | Conversion of data to PBM values.
  pnmString components = wrapText (WrapSettings False False) 70 pnmData
    where pnmData = T.intercalate (T.pack " ") pixels
          pixels  = fmap pnmPixel components

instance PNMPixel (Intersection f a) where
  pnmPixel (Inter Nothing) = T.pack "0"
  pnmPixel _               = T.pack "1"

instance (PNMPixel a) => Show (PNM a) where
  show pnm = unlines $ [header, size, body]
    where header = show . pnmHeader $ pnm
          size   = show . pnmScreen $ pnm
          body   = T.unpack . pnmString . pnmData $ pnm
