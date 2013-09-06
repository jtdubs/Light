module Main
where

import Control.Monad
import Data.List
import Data.Random
import System.Random.MWC (create)

import Light.Sampler.Sampler

main :: IO ()
main = do
  -- putStrLn "graphics_toolkit (\"fltk\");"
  r <- create
  putStrLn "clf"
  sampleFrom r (liftM toCircles (sampleStrataCenters2D 16 16))        >>= drawCircleSamples 2 4 1 "Circle (Center)"
  sampleFrom r (liftM toCircles (sequence $ replicate 256 sample2D))  >>= drawCircleSamples 2 4 2 "Circle (Random)"
  sampleFrom r (liftM toCircles (sampleLHC2D 256))                    >>= drawCircleSamples 2 4 3 "Circle (LHC)"
  sampleFrom r (liftM toCircles (sampleStrata2D 16 16))               >>= drawCircleSamples 2 4 4 "Circle (Strata)"
  sampleFrom r (sampleStrataCenters2D 16 16)                          >>= drawSquareSamples 2 4 5 "Square (Center)"
  sampleFrom r (sequence $ replicate 256 sample2D)                    >>= drawSquareSamples 2 4 6 "Square (Random)"
  sampleFrom r (sampleLHC2D 256)                                      >>= drawSquareSamples 2 4 7 "Square (LHC)"
  sampleFrom r (sampleStrata2D 16 16)                                 >>= drawSquareSamples 2 4 8 "Square (Strata)"

drawCircleSamples :: Int -> Int -> Int -> String -> [(Double, Double)] -> IO ()
drawCircleSamples w h ix title samples = do
  putStrLn $ "subplot (" ++ show w ++ ", " ++ show h ++ ", " ++ show ix ++ ");"
  putStrLn $ "t = linspace(0, 2*pi, 100);"
  putStrLn $ "plot(cos(t), sin(t));"
  drawSamples title (1, 1) samples

drawSquareSamples :: Int -> Int -> Int -> String -> [(Double, Double)] -> IO ()
drawSquareSamples w h ix title samples = do
  putStrLn $ "subplot (" ++ show w ++ ", " ++ show h ++ ", " ++ show ix ++ ");"
  putStrLn $ "rectangle ();"
  drawSamples title (1, 1) samples

drawSamples :: String -> (Double, Double) -> [(Double, Double)] -> IO ()
drawSamples title (w, h) samples = do
  let fw = w / sqrt ((w*w) + (h*h))
  let fh = h / sqrt ((w*w) + (h*h))
  putStrLn   "hold on;"
  putStrLn $ "pbaspect ([" ++ show fw ++ ", " ++ show fh ++ "]);"
  putStrLn $ "axis (\"off\", \"nolabel\");"
  putStrLn $ "title (\"" ++ title ++ "\");"
  putStrLn $ "x = [" ++ intercalate ", " (map (show.fst) samples) ++ "];"
  putStrLn $ "y = [" ++ intercalate ", " (map (show.snd) samples) ++ "];"
  putStrLn   "scatter(x, y, 0.1, \"auto\");"
  putStrLn   "hold off;"
