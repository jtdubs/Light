{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Cylinder
  -- ADT
  ( Cylinder, cylinder, cylinderRadius, cylinderHeight

  -- Default Instances
  , unitCylinder
  )
where

import Control.Lens hiding (transform)

import Light.Math
import Light.Geometry
import Light.Shape

data Cylinder = Cylinder { _cylinderTransform :: Transform, _cylinderRadius :: Double, _cylinderHeight :: Double } deriving (Show, Read)

cylinder :: Double -> Double -> Cylinder
cylinder = Cylinder identityTransform

makeLenses ''Cylinder

unitCylinder :: Cylinder
unitCylinder = cylinder 1 1

instance Transformable Cylinder where
  transform t' (Cylinder t r h) = Cylinder (compose t' t) r h

instance Shape Cylinder where
  shapeTransform = cylinderTransform

  bound (Cylinder _ r h) = fromPoints [ Point (-r) (-r) 0, Point r r h ]

  surfaceArea (Cylinder _ r h) = 2 * pi * r * h

  intersections theRay (Cylinder t r h) = filter f $ quadratic a b c
    where r'     = transform (inverse t) theRay
          rdx    = dx $ rayDirection r'
          rdy    = dy $ rayDirection r'
          rox    = px $ rayOrigin r'
          roy    = py $ rayOrigin r'
          a      = (rdx*rdx) + (rdy*rdy)
          b      = 2 * ((rdx*rox) + (rdy*roy))
          c      = (rox*rox) + (roy*roy) - r*r
          f time = let rz = pz (r' `atTime` time)
                   in time > 0 && rz >= 0 && rz <= h
