module Main where

import Vector
import Geometry
import Camera

camera   = Camera (Vector 0 5 0) (Vector 1 (-1) 1) (Screen 640 480)
triangle = Triangle (Vector 1 0 1) (Vector 2 0 5) (Vector 5 0 2)
gs       = [triangle]

main :: IO ()
main = putStrLn $ render camera gs
