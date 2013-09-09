module Main
where

import Data.List

import Light.Cameras
import Light.Film
import Light.Filters
import Light.Geometry

f :: Film
f = film (16, 12) (boxFilter (1, 1))

p1, p2 :: PerspectiveCamera
p1 = perspectiveCamera f (pi/3)
p2 = perspectiveCamera f (pi/2)

o :: OrthographicCamera
o = orthographicCamera f 1

getRays :: (Camera c) => c -> [Ray]
getRays camera =
  let (fw, fh) = (filmDimensions.cameraFilm) camera
  in [cameraRay camera (fromIntegral x + 0.5, fromIntegral y + 0.5) | x <- [0..fw-1], y <- [0..fh-1]]

main :: IO ()
main = do
  putStrLn "graphics_toolkit (\"fltk\");"
  putStrLn "clf"
  drawPerspectivePlot  1 "Perspective (60)" p1
  drawPerspectivePlot  2 "Perspective (90)" p2
  drawOrthographicPlot 3 "Orthographic"     o 

drawPerspectivePlot :: Int -> String -> PerspectiveCamera -> IO ()
drawPerspectivePlot ix title camera = let fovY   = perspectiveVerticalFOV  camera
                                          (_,fh) = (filmDimensions.cameraFilm) camera
                                      in drawPlot ix title camera (fromIntegral fh / (2 * tan (fovY/2)))

drawOrthographicPlot :: Int -> String -> OrthographicCamera -> IO ()
drawOrthographicPlot ix title camera = let (fw, fh) = (filmDimensions.cameraFilm) camera
									   in drawPlot ix title camera (fromIntegral (min fw fh))

drawPlot :: (Camera c) => Int -> String -> c -> Double -> IO ()
drawPlot ix title camera imagePlaneHeight = do
  putStrLn $ "figure (" ++ show ix ++ ");"
  putStrLn $ "x = linspace (" ++ intercalate ", " (map show [-fw/2, fw/2, fw+1]) ++ ");"
  putStrLn $ "y = linspace (" ++ intercalate ", " (map show [-fh/2, fh/2, fh+1]) ++ ");"
  putStrLn   "[xx, yy] = meshgrid(x, y);"
  putStrLn $ "zz = (xx.*0).+" ++ show imagePlaneHeight ++ ";"
  putStrLn   "mesh (xx, yy, zz);"
  putStrLn   "hold on;"
  putStrLn   "grid off;"
  putStrLn   "box off;"
  putStrLn $ "axis ([" ++ intercalate ", " (map show [-dim/2, dim/2, -dim/2, dim/2, 0, dim]) ++ "], \"square\");"
  putStrLn   "daspect ([1, 1, 1]);"
  putStrLn   "pbaspect ([1, 1, 1]);"
  putStrLn $ "title (\"" ++ title ++ "\");"
  putStrLn $ "ox = [" ++ intercalate ", " (map show ox) ++ "];"
  putStrLn $ "oy = [" ++ intercalate ", " (map show oy) ++ "];"
  putStrLn $ "oz = [" ++ intercalate ", " (map show oz) ++ "];"
  putStrLn $ "dx = [" ++ intercalate ", " (map show vx) ++ "];"
  putStrLn $ "dy = [" ++ intercalate ", " (map show vy) ++ "];"
  putStrLn $ "dz = [" ++ intercalate ", " (map show vz) ++ "];"
  putStrLn   "q = quiver3 (ox, oy, oz, dx, dy, dz, 0);"
  putStrLn   "set (q, \"maxheadsize\", 0);"
  putStrLn   "hold off;"
  where fw             = (fromIntegral.fst.filmDimensions.cameraFilm) camera
        fh             = (fromIntegral.snd.filmDimensions.cameraFilm) camera
        dim            = foldl max 0 [fw, fh, imagePlaneHeight] + 4
        rays           = getRays camera
        ox             = map (px.rayOrigin) rays
        oy             = map (py.rayOrigin) rays
        oz             = map (pz.rayOrigin) rays
        vx             = map (\r -> (dx $ rayDirection r)*imagePlaneHeight*1.2/(dz $ rayDirection r)) rays
        vy             = map (\r -> (dy $ rayDirection r)*imagePlaneHeight*1.2/(dz $ rayDirection r)) rays
        vz             = map (\r -> (dz $ rayDirection r)*imagePlaneHeight*1.2/(dz $ rayDirection r)) rays
