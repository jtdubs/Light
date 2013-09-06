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
  -- sampleFrom r (liftM toCircles (sampleStrataCenters2D 10 10))       >>= drawCircleSamples 1 2 3 1 Nothing "Center"
  -- sampleFrom r (liftM toCircles (sequence $ replicate 100 sample2D)) >>= drawCircleSamples 1 2 3 2 Nothing "Random"
  -- sampleFrom r (liftM toCircles (sampleLHC2D 100))                   >>= drawCircleSamples 1 2 3 3 Nothing "LHC"
  -- sampleFrom r (liftM toCircles (sampleStrata2D 10 10))              >>= drawCircleSamples 1 2 3 4 Nothing "Strata"
  -- sampleFrom r (liftM toCircles (sampleHalton2D 100))                >>= drawCircleSamples 1 2 3 5 Nothing "Halton"
  -- sampleFrom r (liftM toCircles (sampleHammersley2D 100))            >>= drawCircleSamples 1 2 3 6 Nothing "Hammersley"
  sampleFrom r (sampleStrataCenters2D 10 10)                         >>= drawSquareSamples 1 3 3 1 Nothing "Center"
  sampleFrom r (sequence $ replicate 100 sample2D)                   >>= drawSquareSamples 1 3 3 2 Nothing "Random"
  sampleFrom r (sampleLHC2D 100)                                     >>= drawSquareSamples 1 3 3 3 Nothing "LHC"
  sampleFrom r (sampleStrata2D 10 10)                                >>= drawSquareSamples 1 3 3 4 Nothing "Strata"
  sampleFrom r (sampleHalton2D 100)                                  >>= drawSquareSamples 1 3 3 5 Nothing "Halton"
  sampleFrom r (sampleHammersley2D 100)                              >>= drawSquareSamples 1 3 3 6 Nothing "Hammersley"
  sampleFrom r (sample022D 0 0 16)                                   >>= drawSquareSamples 1 3 3 7 Nothing "(0,2)"

drawCircleSamples :: Int -> Int -> Int -> Int -> Maybe (Int, Int) -> String -> [(Double, Double)] -> IO ()
drawCircleSamples f w h ix _ title samples = do
  putStrLn $ "figure (" ++ show f ++ ");"
  putStrLn $ "subplot (" ++ show w ++ ", " ++ show h ++ ", " ++ show ix ++ ");"
  putStrLn $ "t = linspace(0, 2*pi, 100);"
  putStrLn $ "plot(cos(t), sin(t));"
  drawSamples title (1, 1) samples

drawSquareSamples :: Int -> Int -> Int -> Int -> Maybe (Int, Int) -> String -> [(Double, Double)] -> IO ()
drawSquareSamples f w h ix g title samples = do
  putStrLn $ "figure (" ++ show f ++ ");"
  putStrLn $ "subplot (" ++ show w ++ ", " ++ show h ++ ", " ++ show ix ++ ");"
  case g of
    Nothing     -> putStrLn $ "rectangle ();"
    Just (x, y) -> do putStrLn $ "w = " ++ show x ++ ";"
                      putStrLn $ "h = " ++ show y ++ ";"
                      putStrLn   "for x = 0:w-1"
                      putStrLn   "  for y = 0:h-1"
                      putStrLn   "    rectangle (\"position\", [x*(1/w), y*(1/h), 1/w, 1/h]);"
                      putStrLn   "  endfor"
                      putStrLn   "endfor"
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
