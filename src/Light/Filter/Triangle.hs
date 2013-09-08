module Light.Filter.Triangle
  ( TriangleFilter, triangleFilter
  )
where

import Light.Filter

data TriangleFilter = TriangleFilter { triangleFilterExtent :: (Double, Double) }
                    deriving (Show, Read, Eq)

triangleFilter :: (Double, Double) -> TriangleFilter
triangleFilter = TriangleFilter

instance Filter TriangleFilter where
  filterExtent = triangleFilterExtent
  filterWeight (TriangleFilter (w, h)) (x, y) = max 0 $ tx * ty
    where tx = 1 - abs (x / w)
          ty = 1 - abs (y / h)
