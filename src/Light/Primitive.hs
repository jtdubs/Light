{-# LANGUAGE ExistentialQuantification, TemplateHaskell #-}

module Light.Primitive
  ( Primitive, primitive, primitiveShape, primitiveMaterial
  , Material(..)
  )
where

import Control.Lens

import Light.Shape

data Material = Material deriving (Eq, Show, Read)

data Primitive = Primitive { _primitiveShape :: ShapeBox, _primitiveMaterial :: Material } deriving (Show)

makeLenses ''Primitive

primitive :: (Shape s, Show s) => s -> Material -> Primitive
primitive s = Primitive (shapeBox s)

instance Shape Primitive where
  shapeTransform = primitiveShape.shapeTransform
  bound          = bound       . _primitiveShape
  worldBound     = worldBound  . _primitiveShape
  intersects r s = intersects r (s^.primitiveShape)
  intersect  r s = intersect  r (s^.primitiveShape)
  surfaceArea    = surfaceArea . _primitiveShape
