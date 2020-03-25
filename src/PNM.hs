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

-- | Conversion of PNM data to string.
pnmString :: (PNMPixel a) => PNM a -> T.Text
pnmString pnm = wrapText (WrapSettings False False) 70 result
  where result  = T.intercalate (T.pack " ") pixels
        pixels  = fmap pnmPixel (pnmData pnm)

class PNMPixel a where
  pnmPixel :: a -> T.Text

instance PNMPixel Int where
  pnmPixel = T.pack . show

instance PNMPixel (Intersection f a) where
  pnmPixel (Inter Nothing) = T.pack "0"
  pnmPixel _               = T.pack "1"

instance (PNMPixel a) => Show (PNM a) where
  show pnm = unlines $ [header, size, body]
    where header = show . pnmHeader $ pnm
          size   = show . pnmScreen $ pnm
          body   = T.unpack . pnmString $ pnm
