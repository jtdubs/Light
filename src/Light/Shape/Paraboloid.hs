{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Paraboloid
  -- ADT
  ( Paraboloid, paraboloid, paraboloidRadius, paraboloidHeight

  -- Default Instances
  , unitParaboloid
  )
where

import Control.Monad
import Control.Lens hiding (transform)

import Light.Math
import Light.Geometry.AABB
import Light.Geometry.Point
import Light.Geometry.Ray
import Light.Geometry.Transform
import Light.Geometry.Vector
import Light.Shape.Shape

data Paraboloid = Paraboloid { _paraboloidTransform :: Transform, _paraboloidRadius :: Float, _paraboloidHeight :: Float }

paraboloid :: Float -> Float -> Paraboloid
paraboloid = Paraboloid identityTransform

makeLenses ''Paraboloid

unitParaboloid :: Paraboloid
unitParaboloid = paraboloid 1 1

instance Show Paraboloid where
  show (Paraboloid t r h) = concat ["#P(", show t, ", ", show r, ", ", show h, ")"]

instance Transformable Paraboloid where
  transform t' (Paraboloid t r h) = Paraboloid (compose t' t) r h

instance Shape Paraboloid where
  shapeTransform = paraboloidTransform

  bound (Paraboloid _ r h) = fromPoints [ point (-r) (-r) 0, point r r h ]

  surfaceArea (Paraboloid _ r h) = (pi/6) * (r/(h*h)) * ((r*r + 4*h*h) * 3/2 - r*r*r)

  intersect theRay (Paraboloid t r h) = do
    ts <- liftM (filter f) $ quadratic a b c
    guard  $ not (null ts)
    return $ head ts
    where r'     = transform (inverse t) theRay
          rdx    = r'^.rayDirection.dx
          rdy    = r'^.rayDirection.dy
          rdz    = r'^.rayDirection.dz
          rox    = r'^.rayOrigin.px
          roy    = r'^.rayOrigin.py
          roz    = r'^.rayOrigin.pz
          a      = (  h*rdx*rdx +   h*rdy*rdy)/(r*r)
          b      = (2*h*rox*rdx + 2*h*roy*rdy)/(r*r) - rdz
          c      = (  h*rox*rox +   h*roy*roy)/(r*r) - roz
          f time = let rz = (r' `atTime` time)^.pz
                   in time > 0 && rz >= 0 && rz <= h
