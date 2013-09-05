module Main
where

import Control.Monad.Random
import Data.List

import Light.Sampler.Sampler

circleSamples :: (RandomGen g) => Rand g [(Float, Float)]
circleSamples = sequence $ replicate 900 sampleCircle

squareSamples :: (RandomGen g) => Rand g [(Float, Float)]
squareSamples = sequence $ replicate 900 sampleSquare

squareStrataSamples :: (RandomGen g) => Rand g [(Float, Float)]
squareStrataSamples = sampleStrata2D 30 30

circleStrataSamples :: (RandomGen g) => Rand g [(Float, Float)]
circleStrataSamples = sampleStrataCircle 30 30

main :: IO ()
main = do
  putStrLn "graphics_toolkit (\"fltk\");"
  putStrLn "clf"
  drawCircleSamples 1 "Circle (Random)" circleSamples
  drawCircleSamples 2 "Circle (Strata)" circleStrataSamples
  drawSquareSamples 3 "Square (Random)" squareSamples
  drawSquareSamples 4 "Square (Strata)" squareStrataSamples

drawCircleSamples :: Int -> String -> Rand StdGen [(Float, Float)] -> IO ()
drawCircleSamples ix title samples = do
  putStrLn $ "figure (" ++ show ix ++ ");"
  putStrLn $ "t = linspace(0, 2*pi, 100);"
  putStrLn $ "plot(cos(t), sin(t));"
  drawSamples title samples

drawSquareSamples :: Int -> String ->  Rand StdGen [(Float, Float)] -> IO ()
drawSquareSamples ix title samples = do
  putStrLn $ "figure (" ++ show ix ++ ");"
  putStrLn $ "rectangle ();"
  drawSamples title samples

drawSamples :: String -> Rand StdGen [(Float, Float)] -> IO ()
drawSamples title samples = do
  s <- evalRandIO samples
  putStrLn   "hold on;"
  putStrLn   "daspect ([1, 1]);"
  putStrLn   "pbaspect ([1, 1]);"
  putStrLn $ "title (\"" ++ title ++ "\");"
  putStrLn $ "x = [" ++ intercalate ", " (map (show.fst) s) ++ "];"
  putStrLn $ "y = [" ++ intercalate ", " (map (show.snd) s) ++ "];"
  putStrLn   "scatter(x, y);"
  putStrLn   "hold off;"
