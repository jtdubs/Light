module Light.Sampler.Sampler
  ( sampleCirclePolar, sampleCircle, sampleSquare
  , sampleStrata1D, sampleStrata2D
  )
where

import Control.Monad.Random
import Control.Monad

sampleStrata1D :: (RandomGen g) => Int -> Rand g [Float]
sampleStrata1D n = let w = 1 / fromIntegral n
                   in mapM (\i -> getRandomR (w * fromIntegral i, w * fromIntegral (i+1))) [0..n-1]

sampleStrata2D :: (RandomGen g) => Int -> Int -> Rand g [(Float, Float)]
sampleStrata2D n m = sequence [helper (fromIntegral x) (fromIntegral y) | x <- [0..n-1], y <- [0..m-1]]
  where w = 1 / fromIntegral n
        h = 1 / fromIntegral m
        helper x y = do px <- getRandomR (w*x, w*(x+1))
                        py <- getRandomR (h*y, h*(y+1))
                        return (px, py)

sampleSquare :: (RandomGen g) => Rand g (Float, Float)
sampleSquare = do
  x <- getRandomR (0, 1)
  y <- getRandomR (0, 1)
  return (x, y)

sampleCircle :: (RandomGen g) => Rand g (Float, Float)
sampleCircle = do
  (theta, r) <- sampleCirclePolar
  let x = r * cos theta
  let y = r * sin theta
  return (x, y)

sampleCirclePolar :: (RandomGen g) => Rand g (Float, Float)
sampleCirclePolar = do
  theta <- getRandomR (0, 2*pi)
  r     <- liftM sqrt $ getRandomR (0, 1)
  return (theta, r)
