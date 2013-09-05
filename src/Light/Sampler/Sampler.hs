module Light.Sampler.Sampler
  ( sampleCirclePolar, sampleCircle, sampleSquare
  )
where

import Control.Monad.Random
import Control.Monad

-- sampleStrata1D :: (RandomGen g) => Int -> Rand g [Float]
-- sampleStrata1D n = do
--   let w = 1/n
--   mapM (\i -> ...) [1..n]

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
