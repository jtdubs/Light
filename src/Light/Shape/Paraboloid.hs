{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Paraboloid
  -- ADT
  ( Paraboloid, paraboloid, paraboloidRadius, paraboloidHeight

  -- Default Instances
  , unitParaboloid
  )
where

import Control.Lens hiding (transform)

import Light.Math
import Light.Geometry
import Light.Shape

data Paraboloid = Paraboloid { _paraboloidTransform :: Transform, _paraboloidRadius :: Double, _paraboloidHeight :: Double } deriving (Show, Read)

paraboloid :: Double -> Double -> Paraboloid
paraboloid = Paraboloid identityTransform

makeLenses ''Paraboloid

unitParaboloid :: Paraboloid
unitParaboloid = paraboloid 1 1

instance Transformable Paraboloid where
  transform t' (Paraboloid t r h) = Paraboloid (compose t' t) r h

instance Shape Paraboloid where
  shapeTransform = paraboloidTransform

  bound (Paraboloid _ r h) = fromPoints [ Point (-r) (-r) 0, Point r r h ]

  surfaceArea (Paraboloid _ r h) = (pi/6) * (r/(h*h)) * ((r*r + 4*h*h) * 3/2 - r*r*r)

  intersections theRay (Paraboloid t r h) = filter f $ quadratic a b c
    where r'     = transform (inverse t) theRay
          rdx    = dx $ rayDirection r'
          rdy    = dy $ rayDirection r'
          rdz    = dz $ rayDirection r'
          rox    = px $ rayOrigin r'
          roy    = py $ rayOrigin r'
          roz    = pz $ rayOrigin r'
          a      = (  h*rdx*rdx +   h*rdy*rdy)/(r*r)
          b      = (2*h*rox*rdx + 2*h*roy*rdy)/(r*r) - rdz
          c      = (  h*rox*rox +   h*roy*roy)/(r*r) - roz
          f time = let rz = pz (r' `atTime` time)
                   in time > 0 && rz >= 0 && rz <= h
