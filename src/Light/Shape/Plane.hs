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

import Light.Geometry.AABB
import Light.Geometry.Point
import Light.Geometry.Ray
import Light.Geometry.Transform
import Light.Geometry.Vector
import Light.Shape.Shape

data Plane = Plane { _planeTransform :: Transform, _planeHalfWidth :: Double, _planeHalfDepth :: Double } deriving (Show, Read)

plane :: Double -> Double -> Plane
plane = Plane identityTransform

makeLenses ''Plane

unitPlane :: Plane
unitPlane = plane 1 1

instance Transformable Plane where
  transform t' (Plane t w d) = Plane (compose t' t) w d

instance Shape Plane where
  shapeTransform = planeTransform

  bound (Plane _ w d) = fromPoints [ point (-w) (-d) 0, point w d 0 ]

  surfaceArea (Plane _ w d) = 4 * w *d

  intersect theRay (Plane t w d) = do
    guard $ abs rdz > 0.0001
    let time = -roz / rdz
    guard $ time >= 0
    let rt = r' `atTime` time
    guard $ abs (rt^.px) <= w
    guard $ abs (rt^.py) <= d
    return time
    where r'  = transform (inverse t) theRay
          rdz = r'^.rayDirection.dz
          roz = r'^.rayOrigin.pz
