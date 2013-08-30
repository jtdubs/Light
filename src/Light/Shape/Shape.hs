module Light.Shape.Shape
  ( Shape(..)
  )
where

import Control.Lens hiding (transform)
import Data.Maybe

import Light.Geometry.AABB
import Light.Geometry.Transform
import Light.Geometry.Ray

class Shape a where
  shapeTransform :: Lens' a Transform
  bound :: a -> AABB
  worldBound :: a -> AABB
  intersects :: Ray -> a -> Bool
  intersect :: Ray -> a -> Maybe Double
  surfaceArea :: a -> Double

  worldBound s = transform (inverse (s^.shapeTransform)) (bound s)
  intersects r s = isJust $ intersect r s
