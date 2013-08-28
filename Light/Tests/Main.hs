{-# LANGUAGE ParallelListComp #-}

module Main
where

import Control.Lens hiding (transform)
import Control.Monad
import Data.List

import Light.Geometry.Point
import Light.Geometry.Ray
import Light.Geometry.Transform
import Light.Geometry.Vector
import Light.Camera.Camera
import Light.Camera.OrthographicCamera
import Light.Camera.PerspectiveCamera
import Light.Camera.Film

f@(Film fx fy) = Film 16 12
c = perspectiveCamera f (pi/3)

rs = [cameraRay c (fromIntegral x, fromIntegral y) | x <- [0..fx-1], y <- [0..fy-1]]
sz = 6 / tan(pi/6)

main = do
  putStrLn "clf"
  putStrLn "axis ([-8, 8, -8, 8, 0, 16]);"
  putStrLn "x = linspace (-8, 8, 17);"
  putStrLn "y = linspace (-6, 6, 13);"
  putStrLn "[xx, yy] = meshgrid(x, y);"
  putStrLn $ "zz = (xx.*0).+" ++ (show sz) ++ ";"
  putStrLn "mesh (xx, yy, zz);"
  putStrLn "hold on;"
  let ox = map (^.rayOrigin.px) rs
  let oy = map (^.rayOrigin.py) rs
  let oz = map (^.rayOrigin.pz) rs
  let vx = map (\r -> (r^.rayDirection.dx)*sz*1.2/(r^.rayDirection.dz)) rs
  let vy = map (\r -> (r^.rayDirection.dy)*sz*1.2/(r^.rayDirection.dz)) rs
  let vz = map (\r -> (r^.rayDirection.dz)*sz*1.2/(r^.rayDirection.dz)) rs
  putStrLn $ "ox = [" ++ concat (intersperse ", " (map show ox)) ++ "];"
  putStrLn $ "oy = [" ++ concat (intersperse ", " (map show oy)) ++ "];"
  putStrLn $ "oz = [" ++ concat (intersperse ", " (map show oz)) ++ "];"
  putStrLn $ "dx = [" ++ concat (intersperse ", " (map show vx)) ++ "];"
  putStrLn $ "dy = [" ++ concat (intersperse ", " (map show vy)) ++ "];"
  putStrLn $ "dz = [" ++ concat (intersperse ", " (map show vz)) ++ "];"
  putStrLn "q = quiver3 (ox, oy, oz, dx, dy, dz, 0);"
  putStrLn "set (q, \"maxheadsize\", 0);"
  putStrLn "hold off;"
