module Light.Filter.Box
  ( BoxFilter, boxFilter
  )
where

import Light.Filter

data BoxFilter = BoxFilter { boxFilterExtent :: (Double, Double) }
               deriving (Show, Read, Eq)

boxFilter :: (Double, Double) -> BoxFilter
boxFilter = BoxFilter

instance Filter BoxFilter where
  filterExtent = boxFilterExtent
  filterWeight _ _ = 1
