module Light.Shape.Plane
  -- ADT
  ( Plane, plane, planeHalfWidth, planeHalfDepth

  -- Default Instances
  , unitPlane
  )
where

import Control.Monad

import Light.Geometry
import Light.Shape

data Plane = Plane { planeTransform :: Transform
                   , planeHalfWidth :: Double
                   , planeHalfDepth :: Double
                   }
           deriving (Show, Read)

plane :: Double -> Double -> Plane
plane = Plane identityTransform

unitPlane :: Plane
unitPlane = plane 1 1

instance Transformable Plane where
  transform t' (Plane t w d) = Plane (compose t' t) w d

instance Shape Plane where
  shapeTransform = planeTransform

  bound (Plane _ w d) = fromPoints [ Point (-w) (-d) 0, Point w d 0 ]

  surfaceArea (Plane _ w d) = 4 * w *d

  intersect theRay (Plane t w d) = do
    guard $ abs rdz > 0.0001
    let time = -roz / rdz
    guard $ time >= 0
    let rt = r' `atTime` time
    guard $ abs (px rt) <= w
    guard $ abs (py rt) <= d
    return time
    where r'  = transform (inverse t) theRay
          rdz = dz $ rayDirection r'
          roz = pz $ rayOrigin r'
