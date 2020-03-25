module PNM where

import qualified Data.Text as T
import Text.Wrap
import Geometry
import Screen

data PNMHeader = P2 deriving Show

-- | PNM structure.
data PNM a = PNM { pnmHeader :: PNMHeader
                 , pnmScreen :: Screen
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

instance PNMPixel (Intersection f a) where
  pnmPixel (Inter Nothing) = T.pack "50"
  pnmPixel _               = T.pack "200"

instance (PNMPixel a) => Show (PNM a) where
  show pnm = unlines [header, size, maxPx, body]
    where header = show . pnmHeader $ pnm
          size   = show . pnmScreen $ pnm
          maxPx  = show 0xFF
          body   = T.unpack . pnmString $ pnm
