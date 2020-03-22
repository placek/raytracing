module Main where

import System.Environment
import System.IO
import Vector
import Geometry
import Camera
import PBM

camera    = Camera (Vector 0 5 0) (Vector 1 (-1) 1) (Screen 640 480)
triangle1 = Triangle (Vector 1 0 1) (Vector 2 0 5) (Vector 5 0 2)
triangle2 = Triangle (Vector 1 0 1) (Vector 4 0 7) (Vector 7 0 4)
gs        = [triangle1, triangle2]
pbm       = render camera gs

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn pbm
    else writeFile (head args) pbm
  return ()
