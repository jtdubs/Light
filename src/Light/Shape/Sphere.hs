module Light.Shape.Sphere
  -- ADT
  ( Sphere, sphere, sphereRadius

  -- Default Instances
  , unitSphere
  )
where

import Light.Math
import Light.Geometry
import Light.Shape

data Sphere = Sphere { sphereTransform :: Transform
                     , sphereRadius    :: Double
                     }
            deriving (Show, Read)

sphere :: Double -> Sphere
sphere = Sphere identityTransform

unitSphere :: Sphere
unitSphere = sphere 1

instance Transformable Sphere where
  transform t' (Sphere t r) = Sphere (compose t' t) r

instance Shape Sphere where
  shapeTransform = sphereTransform

  bound (Sphere _ r) = fromPoints [ Point (-r) (-r) (-r), Point r r r ]

  surfaceArea (Sphere _ r) = 4 * pi * r * r

  intersections theRay (Sphere t r) = filter (> 0) $ quadratic a b c
    where r' = transform (inverse t) theRay
          a  = magnitudeSquaredV $ rayDirection r'
          b  = 2 * (rayDirection r' ^.^ (rayOrigin r' .-. originPoint))
          c  = distanceSquared (rayOrigin r') originPoint - r*r
