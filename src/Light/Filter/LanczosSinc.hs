module Light.Filter.LanczosSinc
  ( LanczosSincFilter, lanczosSincFilter
  )
where

import Light.Filter

data LanczosSincFilter = LanczosSincFilter { lanczosSincFilterExtent :: (Double, Double)
                                           , lanczosSincFilterTau    :: Double
                                           }
                       deriving (Show, Read, Eq)

lanczosSincFilter :: (Double, Double) -> Double -> LanczosSincFilter
lanczosSincFilter = LanczosSincFilter

instance Filter LanczosSincFilter where
  filterExtent = lanczosSincFilterExtent
  filterWeight (LanczosSincFilter (w, h) t) (x, y) = (helper (abs (x/w))) * (helper (abs (y/h)))
    where helper x | x < 0.00001 = 1
                   | x > 1       = 0
                   | otherwise   = let xp      = x * pi
                                       sinc    = (sin (xp * t)) / (xp * t)
                                       lanczos = (sin xp) / (xp)
                                   in sinc * lanczos
