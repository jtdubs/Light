module Main
where

import Data.List
import Light.Filters

main :: IO ()
main = do
  putStrLn "clf"
  test 1 "Box"          $ boxFilter         (2, 2)
  test 2 "Triangle"     $ triangleFilter    (2, 2)
  test 3 "Gaussian"     $ gaussianFilter    (2, 2) 0.5
  test 4 "Mitchell"     $ mitchellFilter    (3, 3) 0.5 0.25
  test 5 "Lanczos Sinc" $ lanczosSincFilter (3, 3) pi

test :: (Filter f) => Int -> String -> f -> IO ()
test ix title f = do
  putStrLn $ "figure (" ++ show ix ++ ");"
  -- putStrLn $ "subplot (" ++ show w ++ ", " ++ show h ++ ", " ++ show ix ++ ");"
  putStrLn   "hold on;"
  putStrLn $ "pbaspect ([1, 1]);"
  putStrLn   "axis (\"off\", \"nolabel\");"
  putStrLn $ "title (\"" ++ title ++ "\");"
  putStrLn   "x = linspace(-3, 3, 61);"
  putStrLn   "y = linspace(-3, 3, 61);"
  putStrLn   "[xx, yy] = meshgrid(x, y);"
  putStrLn $ "z = [" ++ z ++ "];"
  putStrLn   "surf(xx, yy, z);"
  putStrLn   "hold off;"
  where z = intercalate "; "
            [ intercalate ", "
              [ show $ f' (x/100) (y/100)
              | x <- [-300, -290 .. 300] ]
            | y <- [-300, -290 .. 300] ]
        f' x y = if (x < -2 || y < -2 || x > 2 || y > 2) then 0 else filterWeight f (x, y)
