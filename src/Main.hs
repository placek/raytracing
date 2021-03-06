module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Text.ParserCombinators.Parsec
import           Vector
import           Geometry
import           Camera
import           PNM
import           Screen
import           STL

stlResult :: T.Text -> Either ParseError STL
stlResult stl = parse parseSTL "(stdin)" (T.unpack stl)

stlToPbm :: T.Text -> T.Text
stlToPbm stl = T.pack $ parseResult (stlResult stl)
  where
    cp                             = Vector (-40.0) (-30.0) 90.0
    camera                         = Camera cp (norm $ mempty |-| cp) (Screen 640 480)
    gray                           = Vector 0.1 0.1 0.1
    white                          = Vector 0.5 0.5 0.5
    light                          = Light (Vector 0.0 0.0 20.0) gray white white
    gs                             = parse parseSTL "(stdin)" (T.unpack stl)
    parseResult (Right (STL _ gs)) = render camera light (fmap stlCoerce gs)
    parseResult (Left a)           = show a

main :: IO ()
main = TIO.interact stlToPbm
