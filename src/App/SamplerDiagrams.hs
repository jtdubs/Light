module Main
where

import Control.Monad.Random
import Data.List

import Light.Sampler.Sampler

main :: IO ()
main = do
  -- putStrLn "graphics_toolkit (\"fltk\");"
  putStrLn "clf"
  drawCircleSamples 3 3 1 "Circle (Center)" $ sampleStrataCentersCircle 30 30
  drawCircleSamples 3 3 2 "Circle (Strata)" $ sampleStrataCircle 30 30
  drawCircleSamples 3 3 3 "Circle (Random)" $ sequence $ replicate 900 sampleCircle
  drawSquareSamples 3 3 4 "Square (Center)" $ sampleStrataCentersSquare 30 30
  drawSquareSamples 3 3 5 "Square (Strata)" $ sampleStrataSquare 30 30
  drawSquareSamples 3 3 6 "Square (Random)" $ sequence $ replicate 900 sampleSquare
  drawPolarSamples  3 3 7 "Polar  (Center)" $ sampleStrataCentersPolar 30 30
  drawPolarSamples  3 3 8 "Polar  (Strata)" $ sampleStrataPolar 30 30
  drawPolarSamples  3 3 9 "Polar  (Random)" $ sequence $ replicate 900 samplePolar

drawCircleSamples :: Int -> Int -> Int -> String -> Rand StdGen [(Float, Float)] -> IO ()
drawCircleSamples w h ix title samples = do
  putStrLn $ "subplot (" ++ show w ++ ", " ++ show h ++ ", " ++ show ix ++ ");"
  putStrLn $ "t = linspace(0, 2*pi, 100);"
  putStrLn $ "plot(cos(t), sin(t));"
  drawSamples title (1, 1) samples

drawSquareSamples :: Int -> Int -> Int -> String ->  Rand StdGen [(Float, Float)] -> IO ()
drawSquareSamples w h ix title samples = do
  putStrLn $ "subplot (" ++ show w ++ ", " ++ show h ++ ", " ++ show ix ++ ");"
  putStrLn $ "rectangle ();"
  drawSamples title (1, 1) samples

drawPolarSamples :: Int -> Int -> Int -> String ->  Rand StdGen [(Float, Float)] -> IO ()
drawPolarSamples w h ix title samples = do
  putStrLn $ "subplot (" ++ show w ++ ", " ++ show h ++ ", " ++ show ix ++ ");"
  putStrLn $ "rectangle (\"Position\", [0, 0, 2*pi, 1]);"
  drawSamples title (2*pi, 1) samples

drawSamples :: String -> (Float, Float) -> Rand StdGen [(Float, Float)] -> IO ()
drawSamples title (w, h) samples = do
  let fw = w / sqrt ((w*w) + (h*h))
  let fh = h / sqrt ((w*w) + (h*h))
  s <- evalRandIO samples
  putStrLn   "hold on;"
  -- putStrLn $ "daspect ([" ++ show fw ++ ", " ++ show fh ++ "]);"
  putStrLn $ "pbaspect ([" ++ show fw ++ ", " ++ show fh ++ "]);"
  putStrLn $ "axis (\"off\", \"nolabel\");"
  putStrLn $ "title (\"" ++ title ++ "\");"
  putStrLn $ "x = [" ++ intercalate ", " (map (show.fst) s) ++ "];"
  putStrLn $ "y = [" ++ intercalate ", " (map (show.snd) s) ++ "];"
  putStrLn   "scatter(x, y, 0.1, \"auto\");"
  putStrLn   "hold off;"
