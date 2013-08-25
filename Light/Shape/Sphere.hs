{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Sphere
    -- ADT
    ( Sphere, sphere, sphereTransform, sphereRadius

    -- Default Instances
    , unitSphere
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

data Sphere = Sphere { _sphereTransform :: Transform, _sphereRadius :: Float }

sphere = Sphere identityTransform

makeLenses ''Sphere

unitSphere = sphere 1

instance Show Sphere where
  show (Sphere t r) = concat ["#S(", show t, ", ", show r, ")"]

instance Transformable Sphere where
  transform t' (Sphere t r) = Sphere (compose t' t) r

instance Shape Sphere where
  shapeTransform = sphereTransform

  bound (Sphere _ r) = fromPoints [ point (-r) (-r) (-r), point r r r ]

  surfaceArea (Sphere _ r) = 4 * pi * r * r

  intersect ray (Sphere t r) = do
    ts <- liftM (filter (> 0)) $ quadratic a b c
    guard  $ length ts > 0
    return $ head ts
    where r' = transform (inverse t) ray
          a  = magnitudeSquaredV $ r'^.rayDirection
          b  = 2 * ((r'^.rayDirection) ^.^ ((r'^.rayOrigin) .-. originPoint))
          c  = (distanceSquared (r'^.rayOrigin) originPoint) - (r*r)
