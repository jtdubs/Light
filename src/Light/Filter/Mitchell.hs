module Light.Filter.Mitchell
  ( MitchellFilter, mitchellFilter
  )
where

import Light.Filter

data MitchellFilter = MitchellFilter { mitchellFilterExtent :: (Double, Double)
                                     , mitchellFilterB      :: Double
                                     , mitchellFilterC      :: Double
                                     }
                    deriving (Show, Read, Eq)

mitchellFilter :: (Double, Double) -> Double -> Double -> MitchellFilter
mitchellFilter = MitchellFilter

instance Filter MitchellFilter where
  filterExtent = mitchellFilterExtent
  filterWeight (MitchellFilter (w, h) b c) (x, y) = helper (x/w) * helper (y/h)
    where helper x = let x' = abs (2 * x)
                     in if (x' > 1)
                        then ( ( -1*b -   6*c) * x'*x'*x'
                             + (  6*b +  30*c) * x'*x'
                             + (-12*b -  48*c) * x'
                             + (  8*b +  24*c) ) / 6
                        else ( ( 12 -  9*b -  6*c) * x'*x'*x'
                             + (-18 + 12*b +  6*c) * x'*x'
                             + (  6 -  2*b       ) ) / 6
