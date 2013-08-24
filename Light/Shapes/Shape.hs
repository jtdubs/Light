{-# LANGUAGE TemplateHaskell #-}

module Light.Shapes.Shape (Shape(..))
where

import Control.Lens hiding (transform)
import Data.Maybe

import Light.Geometry.Ray
import Light.Geometry.Transform
import Light.Geometry.AABB

class Shape a where
  shapeTransform :: Lens' a Transform
  shapeBound :: a -> AABB
  shapeWorldBound :: a -> AABB
  shapeIntersects :: Ray -> a -> Bool
  shapeIntersect :: Ray -> a -> Maybe Float
  shapeSurfaceArea :: a -> Float

  shapeWorldBound s = transform (inverse (s^.shapeTransform)) (shapeBound s)
  shapeIntersects r s = isJust $ shapeIntersect r s
