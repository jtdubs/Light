module Light.Sampler.Sampler
  ( samplePolar, sampleCircle, sampleSquare
  , sampleStrata1D, sampleStrata2D, sampleStrataCircle, sampleStrataPolar
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

sampleStrataCircle :: (RandomGen g) => Int -> Int -> Rand g [(Float, Float)]
sampleStrataCircle n m = do s <- sampleStrataPolar n m
                            return $ map (\ (t, r) -> (r * cos t, r * sin t)) s

sampleStrataPolar :: (RandomGen g) => Int -> Int -> Rand g [(Float, Float)]
sampleStrataPolar n m = sequence [helper (fromIntegral t) (fromIntegral r) | t <- [0..n-1], r <- [0..m-1]]
  where w = (2 * pi) / fromIntegral n
        h = 1 / fromIntegral m
        helper t r = do pt <- getRandomR (w*t, w*(t+1))
                        pr <- getRandomR (h*r, h*(r+1))
                        return (pt, sqrt pr)

sampleSquare :: (RandomGen g) => Rand g (Float, Float)
sampleSquare = do
  x <- getRandomR (0, 1)
  y <- getRandomR (0, 1)
  return (x, y)

sampleCircle :: (RandomGen g) => Rand g (Float, Float)
sampleCircle = do
  (theta, r) <- samplePolar
  let x = r * cos theta
  let y = r * sin theta
  return (x, y)

samplePolar :: (RandomGen g) => Rand g (Float, Float)
samplePolar = do
  theta <- getRandomR (0, 2*pi)
  r     <- liftM sqrt $ getRandomR (0, 1)
  return (theta, r)
