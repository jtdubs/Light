{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Plane
  -- ADT
  ( Plane, plane, planeHalfWidth, planeHalfDepth

  -- Default Instances
  , unitPlane
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

data Plane = Plane { _planeTransform :: Transform, _planeHalfWidth :: Float, _planeHalfDepth :: Float }

plane :: Float -> Float -> Plane
plane = Plane identityTransform

makeLenses ''Plane

unitPlane :: Plane
unitPlane = plane 1 1

instance Show Plane where
  show (Plane t w d) = concat ["#D(", show t, ", ", show w, ", ", show d, ")"]

instance Transformable Plane where
  transform t' (Plane t w d) = Plane (compose t' t) w d

instance Shape Plane where
  shapeTransform = planeTransform

  bound (Plane _ w d) = fromPoints [ point (-w) (-d) 0, point w d 0 ]

  surfaceArea (Plane _ w d) = 4 * w *d

  intersect ray (Plane t w d) = do
    guard $ abs rdz > 0.0001
    let t = -roz / rdz
    guard $ t >= 0
    let rt = r' `atTime` t
    guard $ abs (rt^.px) <= w
    guard $ abs (rt^.py) <= d
    return t
    where r' = transform (inverse t) ray
          rdz = r'^.rayDirection.dz
          roz = r'^.rayOrigin.pz
