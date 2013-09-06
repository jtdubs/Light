module Light.Sampler.Sampler
  ( sample1D, sampleStrata1D, sampleStrataCenters1D, sampleHalton1D, sampleHammersley1D
  , sample2D, sampleStrata2D, sampleStrataCenters2D, sampleHalton2D, sampleHammersley2D, sampleLHC2D, sample022D
  , toCircle, toCircles
  )
where

import Control.Monad
import Data.Word
import Data.Random
import Light.Math

sample1D :: RVar Double
sample1D = uniform 0 1

sampleStrata1D :: Int -> RVar [Double]
sampleStrata1D n = sequence [helper (fromIntegral x) | x <- [0..n-1]]
  where w = 1 / fromIntegral n
        helper x = uniform (w*x) (w*(x+1))

sampleStrataCenters1D :: Int -> RVar [Double]
sampleStrataCenters1D n = sequence [helper (fromIntegral x) | x <- [0..n-1]]
  where w = 1 / fromIntegral n
        helper x = return $ w*x + w/2

sampleHalton1D :: Int -> RVar [Double]
sampleHalton1D n = return [radicalInverse i 2 | i <- [1..n]]

sampleHammersley1D :: Int -> RVar [Double]
sampleHammersley1D n = return [fromIntegral (i-1) / fromIntegral n | i <- [1..n]]


sample2D :: RVar (Double, Double)
sample2D = liftM2 (,) sample1D sample1D

sampleStrata2D :: Int -> Int -> RVar [(Double, Double)]
sampleStrata2D n m = sequence [helper (fromIntegral x) (fromIntegral y) | x <- [0..n-1], y <- [0..m-1]]
  where w = 1 / fromIntegral n
        h = 1 / fromIntegral m
        helper x y = do px <- uniform (w*x) (w*(x+1))
                        py <- uniform (h*y) (h*(y+1))
                        return (px, py)

sampleStrataCenters2D :: Int -> Int -> RVar [(Double, Double)]
sampleStrataCenters2D n m = sequence [helper (fromIntegral x) (fromIntegral y) | x <- [0..n-1], y <- [0..m-1]]
  where w = 1 / fromIntegral n
        h = 1 / fromIntegral m
        helper x y = return (w*x + w/2, h*y + h/2)

sampleHalton2D :: Int -> RVar [(Double, Double)]
sampleHalton2D n = return [(radicalInverse i 2, radicalInverse i 3) | i <- [1..n]]

sampleHammersley2D :: Int -> RVar [(Double, Double)]
sampleHammersley2D n = return [(radicalInverse i 2, fromIntegral (i-1) / fromIntegral n) | i <- [1..n]]

sampleLHC2D :: Int -> RVar [(Double, Double)]
sampleLHC2D n = liftM2 (zipWith (,)) (sampleStrata1D n) (sampleStrata1D n >>= shuffle)

sample022D :: Word32 -> Word32 -> Word32 -> RVar [(Double, Double)]
sample022D s1 s2 n = return [(vanDerCorput i s1, sobol i s2) | i <- [1..n]]


toCircle :: (Double, Double) -> (Double, Double)
toCircle (x, y) = let (t, r) = (x * 2 * pi, sqrt y)
                  in (r * cos t, r * sin t)

toCircles :: [(Double, Double)] -> [(Double, Double)]
toCircles = map toCircle
