module PNM where

import qualified Data.Text as T
import Text.Wrap
import Geometry
import Screen

data PNMHeader = P2 | P3 deriving Show

-- | PNM structure.
data PNM a = PNM { pnmHeader :: PNMHeader
                 , pnmMax    :: Int
                 , pnmScreen :: Screen
                 , pnmData   :: [a]
                 }

-- | Conversion of PNM data to string.
pnmString :: (PNMPixel a) => PNM a -> T.Text
pnmString pnm = wrapText (WrapSettings False False) 70 result
  where result  = T.intercalate (T.pack " ") pixels
        pixels  = fmap (pnmPixel (pnmMax pnm)) (pnmData pnm)

class PNMPixel a where
  pnmPixel :: Int -> a -> T.Text

instance PNMPixel (Intersection f a) where
  pnmPixel mp (Inter Nothing) = T.pack "0"
  pnmPixel mp _               = T.pack . show $ max mp 65535

instance (PNMPixel a) => Show (PNM a) where
  show pnm = unlines [header, size, maxPx, body]
    where header = show . pnmHeader $ pnm
          size   = show . pnmScreen $ pnm
          maxPx  = show . pnmMax $ pnm
          body   = T.unpack . pnmString $ pnm
