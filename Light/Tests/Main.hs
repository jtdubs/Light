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
p1 = perspectiveCamera f (pi/3)
p2 = perspectiveCamera f (pi/2)
o = orthographicCamera f 1

getRays camera =
  let (Film fx fy) = camera^.cameraFilm
  in [cameraRay camera (fromIntegral x, fromIntegral y) | x <- [0..fx-1], y <- [0..fy-1]]

main = do
  putStrLn "graphics_toolkit (\"fltk\");"
  putStrLn "clf"
  drawPerspectivePlot  1 "Perspective (60)" p1
  drawPerspectivePlot  2 "Perspective (90)" p2
  drawOrthographicPlot 3 "Orthographic"     o 

drawPerspectivePlot ix title camera = let fovY        = camera^.perspectiveVerticalFOV
                                          (Film _ fy) = camera^.cameraFilm
                                      in drawPlot ix title camera ((fromIntegral fy) / (2 * tan (fovY/2)))

drawOrthographicPlot ix title camera = let (Film fx fy) = camera^.cameraFilm
									   in drawPlot ix title camera (fromIntegral (min fx fy))

drawPlot :: (Camera c) => Int -> String -> c -> Float -> IO ()
drawPlot ix title camera imagePlaneHeight = do
  let (Film fx' fy') = camera^.cameraFilm
  let fx             = fromIntegral fx'
  let fy             = fromIntegral fy'
  let dim            = (foldl max 0 [fx, fy, imagePlaneHeight]) + 4
  let rays           = getRays camera
  let ox             = map (^.rayOrigin.px) rays
  let oy             = map (^.rayOrigin.py) rays
  let oz             = map (^.rayOrigin.pz) rays
  let vx             = map (\r -> (r^.rayDirection.dx)*imagePlaneHeight*1.2/(r^.rayDirection.dz)) rays
  let vy             = map (\r -> (r^.rayDirection.dy)*imagePlaneHeight*1.2/(r^.rayDirection.dz)) rays
  let vz             = map (\r -> (r^.rayDirection.dz)*imagePlaneHeight*1.2/(r^.rayDirection.dz)) rays

  putStrLn $ "figure (" ++ show ix ++ ");"
  putStrLn $ "x = linspace (" ++ concat (intersperse ", " (map show [-fx/2, fx/2, fx+1])) ++ ");"
  putStrLn $ "y = linspace (" ++ concat (intersperse ", " (map show [-fy/2, fy/2, fy+1])) ++ ");"
  putStrLn $ "[xx, yy] = meshgrid(x, y);"
  putStrLn $ "zz = (xx.*0).+" ++ (show imagePlaneHeight) ++ ";"
  putStrLn $ "mesh (xx, yy, zz);"
  putStrLn $ "hold on;"
  putStrLn $ "grid off;"
  putStrLn $ "box off;"
  putStrLn $ "axis ([" ++ concat (intersperse ", " (map show [-dim/2, dim/2, -dim/2, dim/2, 0, dim])) ++ "], \"square\");"
  putStrLn $ "daspect ([1, 1, 1]);"
  putStrLn $ "pbaspect ([1, 1, 1]);"
  putStrLn $ "title (\"" ++ title ++ "\");"
  putStrLn $ "ox = [" ++ concat (intersperse ", " (map show ox)) ++ "];"
  putStrLn $ "oy = [" ++ concat (intersperse ", " (map show oy)) ++ "];"
  putStrLn $ "oz = [" ++ concat (intersperse ", " (map show oz)) ++ "];"
  putStrLn $ "dx = [" ++ concat (intersperse ", " (map show vx)) ++ "];"
  putStrLn $ "dy = [" ++ concat (intersperse ", " (map show vy)) ++ "];"
  putStrLn $ "dz = [" ++ concat (intersperse ", " (map show vz)) ++ "];"
  putStrLn $ "q = quiver3 (ox, oy, oz, dx, dy, dz, 0);"
  putStrLn $ "set (q, \"maxheadsize\", 0);"
  putStrLn $ "hold off;"
