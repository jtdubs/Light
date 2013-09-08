module Light.Filter.Gaussian
  ( GaussianFilter, gaussianFilter
  )
where

import Light.Filter

data GaussianFilter = GaussianFilter { gaussianFilterExtent   :: (Double, Double)
                                     , gaussianFilterAlpha    :: Double
                                     , gaussianFilterBaseline :: (Double, Double)
                                     }
                    deriving (Show, Read, Eq)

gaussianFilter :: (Double, Double) -> Double -> GaussianFilter
gaussianFilter (w, h) a = GaussianFilter (w, h) a (exp (-a * w * w), exp (-a * h * h))

instance Filter GaussianFilter where
  filterExtent = gaussianFilterExtent
  filterWeight (GaussianFilter _ a (bx, by)) (x, y) = (max 0 gx) * (max 0 gy)
    where gx = exp (-a * x * x) - bx
          gy = exp (-a * y * y) - by
