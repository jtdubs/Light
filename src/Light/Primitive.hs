{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}

module Light.Primitive
  ( Primitive, primitive, primitiveShape, primitiveMaterial
  , Material(..)
  )
where

import Light.Shape
import Light.Geometry.Transform

data Material = Material deriving (Eq, Show, Read)

data Primitive = Primitive { primitiveShape    :: ShapeBox
                           , primitiveMaterial :: Material
                           }
               deriving (Show)

primitive :: (Shape s, Transformable s, Show s) => s -> Material -> Primitive
primitive s = Primitive (shapeBox s)

instance Shape Primitive where
  shapeTransform = shapeTransform . primitiveShape
  bound          = bound          . primitiveShape
  worldBound     = worldBound     . primitiveShape
  surfaceArea    = surfaceArea    . primitiveShape
  intersects r s = intersects r (primitiveShape s)
  intersect  r s = intersect  r (primitiveShape s)
