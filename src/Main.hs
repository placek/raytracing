module Main where

import System.Environment
import System.IO
import Vector
import Geometry
import Camera

camera   = Camera (Vector 0 5 0) (Vector 1 (-1) 1) (Screen 640 480)
triangle = Triangle (Vector 1 0 1) (Vector 2 0 5) (Vector 5 0 2)
gs       = [triangle]
pbm      = render camera gs

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn pbm
    else writeFile (head args) pbm
  return ()
