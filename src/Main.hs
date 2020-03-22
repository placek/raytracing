module Main where

import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Vector
import Geometry
import Camera
import PBM
import STL

stlExampleString = "solid cube_corner\n\
\  facet normal 0.0 -1.0 0.0\n\
\    outer loop\n\
\      vertex 0.0 0.0 0.0\n\
\      vertex 1.0 0.0 0.0\n\
\      vertex 0.0 0.0 1.0\n\
\    endloop\n\
\  endfacet\n\
\  facet normal 0.0 0.0 -1.0\n\
\    outer loop\n\
\      vertex 0.0 0.0 0.0\n\
\      vertex 0.0 1.0 0.0\n\
\      vertex 1.0 0.0 0.0\n\
\    endloop\n\
\  endfacet\n\
\  facet normal -1.0 0.0 0.0\n\
\    outer loop\n\
\      vertex 0.0 0.0 0.0\n\
\      vertex 0.0 0.0 1.0\n\
\      vertex 0.0 1.0 0.0\n\
\    endloop\n\
\  endfacet\n\
\  facet normal 0.577 0.577 0.577\n\
\    outer loop\n\
\      vertex 1.0 0.0 0.0\n\
\      vertex 0.0 1.0 0.0\n\
\      vertex 0.0 0.0 1.0\n\
\    endloop\n\
\  endfacet\n\
\endsolid"

cp     = Vector (-10.0) 0.0 0.0
camera = Camera (Vector (-10.0) 0.0 0.0) (norm $ mempty |-| cp) (Screen 640 480)
gs     = unwrap $ parse parseSTL "example" stlExampleString
pbm    = render camera gs

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn pbm
    else writeFile (head args) pbm
  return ()
