{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Triangle
  -- ADT
  ( Triangle, triangle, triangleTransform, triangleVertices

  -- Default Instances
  , unitTriangle
  )
where

import Control.Monad
import Control.Lens hiding (transform)
import Control.Lens.TH
import Data.List

import Light.Math
import Light.Geometry.AABB
import Light.Geometry.Point
import Light.Geometry.Ray
import Light.Geometry.Transform
import Light.Geometry.Vector
import Light.Shape.Shape

data Triangle = Triangle { _triangleTransform :: Transform, _triangleVertices :: [Point] }

triangle = Triangle identityTransform

makeLenses ''Triangle

unitTriangle = triangle [originPoint, originPoint .+^ unitXVector, originPoint .+^ unitYVector]

instance Show Triangle where
  show (Triangle t ps) = concat ["#T(", show t, ", ", show ps, ")"]

instance Transformable Triangle where
  transform t' (Triangle t ps) = Triangle (compose t' t) ps

instance Shape Triangle where
  shapeTransform = triangleTransform

  bound (Triangle _ ps) = fromPoints ps

  surfaceArea (Triangle _ [a, b, c]) = 0.5 * magnitudeV (cross (b .-. a) (c .-. a))

  intersect ray (Triangle t [v0, v1, v2]) = do
    guard $ a /= 0
    guard $ u >= 0 && u <= 1
    guard $ v >= 0 && (u + v) <= 1
    guard $ t' >= 0
    return t'
    where r' = transform (inverse t) ray
          p  = r'^.rayOrigin
          d  = r'^.rayDirection
          e1 = v1 .-. v0
          e2 = v2 .-. v0
          h  = cross d e2
          a  = e1 ^.^ h
          f  = 1/a
          s  = p .-. v0
          u  = f * (s ^.^ h)
          q  = cross s e1
          v  = f * (d ^.^ q)
          t' = f * (e2 ^.^ q)
