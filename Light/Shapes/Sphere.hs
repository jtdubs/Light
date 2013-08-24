{-# LANGUAGE TemplateHaskell #-}

module Light.Shapes.Sphere
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

import Light.Geometry.AABB
import Light.Geometry.Point
import Light.Geometry.Ray
import Light.Geometry.Transform
import Light.Geometry.Vector
import Light.Shapes.Shape

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

  shapeBound (Sphere _ r) = fromPoints [ point (-r) (-r) (-r), point r r r ]

  shapeSurfaceArea (Sphere _ r) = 4 * pi * r * r

  shapeIntersect ray (Sphere t r) = do
    ts <- liftM (filter (> 0)) $ quadratic a b c
    guard  $ length ts > 0
    return $ head ts
    where r' = transform (inverse t) ray
          a  = magnitudeSquaredV $ r'^.rayDirection
          b  = 2 * ((r'^.rayDirection) ^.^ ((r'^.rayOrigin) .-. originPoint))
          c  = (distanceSquared (r'^.rayOrigin) originPoint) - (r*r)

quadratic a b c = if d < 0
                  then Nothing
                  else Just $ sort $ [ q/a, c/q ]
  where d = b*b - 4*a*c
        q = if b < 0
            then -(b - sqrt d)/2
            else -(b + sqrt d)/2
