{-# LANGUAGE ExistentialQuantification #-}

module Light.Shape.Shape
  ( Shape(..), ShapeBox, shapeBox
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
  intersect :: Ray -> a -> Maybe Double
  intersects :: Ray -> a -> Bool
  intersections :: Ray -> a -> [Double]
  surfaceArea :: a -> Double

  worldBound s = transform (s^.shapeTransform) (bound s)
  intersect r s = let ts = intersections r s
                  in if null ts
                     then Nothing
                     else Just (head ts)
  intersects r s = isJust $ intersect r s
  intersections r s = case intersect r s of
                        Just t  -> [t]
                        Nothing -> []

data ShapeBox = forall s. (Shape s, Show s) => ShapeBox s

shapeBox :: (Shape s, Show s) => s -> ShapeBox
shapeBox = ShapeBox

instance Show ShapeBox where
  show (ShapeBox s) = show s

instance Shape ShapeBox where
  shapeTransform = lens (\ (ShapeBox s)   -> s^.shapeTransform)
                        (\ (ShapeBox s) t -> ShapeBox $ (shapeTransform .~ t) s)
  bound (ShapeBox s) = bound s
  worldBound (ShapeBox s) = worldBound s
  intersect r (ShapeBox s) = intersect r s
  intersects r (ShapeBox s) = intersects r s
  intersections r (ShapeBox s) = intersections r s
  surfaceArea (ShapeBox s) = surfaceArea s
