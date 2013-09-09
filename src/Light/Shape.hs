{-# LANGUAGE ExistentialQuantification #-}

module Light.Shape
  ( Shape(..), ShapeBox, shapeBox
  )
where

import Data.Maybe

import Light.Geometry

class Shape a where
  shapeTransform :: a -> Transform
  bound          :: a -> AABB
  worldBound     :: a -> AABB
  intersect      :: Ray -> a -> Maybe Double
  intersects     :: Ray -> a -> Bool
  intersections  :: Ray -> a -> [Double]
  surfaceArea    :: a -> Double

  worldBound s = transform (shapeTransform s) (bound s)
  intersect r s = let ts = intersections r s
                  in if null ts
                     then Nothing
                     else Just (head ts)
  intersects r s = isJust $ intersect r s
  intersections r s = case intersect r s of
                        Just t  -> [t]
                        Nothing -> []

data ShapeBox = forall s. (Shape s, Transformable s, Show s) => ShapeBox s

shapeBox :: (Shape s, Transformable s, Show s) => s -> ShapeBox
shapeBox = ShapeBox

instance Show ShapeBox where
  show (ShapeBox s) = show s

instance Shape ShapeBox where
  shapeTransform (ShapeBox s) = shapeTransform s
  bound (ShapeBox s) = bound s
  worldBound (ShapeBox s) = worldBound s
  intersect r (ShapeBox s) = intersect r s
  intersects r (ShapeBox s) = intersects r s
  intersections r (ShapeBox s) = intersections r s
  surfaceArea (ShapeBox s) = surfaceArea s

instance Transformable ShapeBox where
  transform t' (ShapeBox s) = ShapeBox (transform t' s)
