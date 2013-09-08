{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Cone
  -- ADT
  ( Cone, cone, coneRadius, coneHeight

  -- Default Instances
  , unitCone
  )
where

import Control.Lens hiding (transform)

import Light.Math
import Light.Geometry
import Light.Shape

data Cone = Cone { _coneTransform :: Transform, _coneRadius :: Double, _coneHeight :: Double } deriving (Show, Read)

cone :: Double -> Double -> Cone
cone = Cone identityTransform

makeLenses ''Cone

unitCone :: Cone
unitCone = cone 1 1

instance Transformable Cone where
  transform t' (Cone t r h) = Cone (compose t' t) r h

instance Shape Cone where
  shapeTransform = coneTransform

  bound (Cone _ r h) = fromPoints [ Point (-r) (-r) 0, Point r r h ]

  surfaceArea (Cone _ r h) = pi * r * sqrt (r*r + h*h)

  intersections theRay (Cone t r h) = filter f $ quadratic a b c
    where r'     = transform (inverse t) theRay
          rdx    = dx $ rayDirection r'
          rdy    = dy $ rayDirection r'
          rdz    = dz $ rayDirection r'
          rox    = px $ rayOrigin r'
          roy    = py $ rayOrigin r'
          roz    = pz $ rayOrigin r'
          a      = (  h*h*rdx*rdx +   h*h*rdy*rdy)/(r*r) + (  -rdz*rdz)
          b      = (2*h*h*rox*rdx + 2*h*h*roy*rdy)/(r*r) + (-2*roz*rdz + 2*rdz*h)
          c      = (  h*h*rox*rox +   h*h*roy*roy)/(r*r) + (  -roz*roz + 2*roz*h - h*h)
          f time = let rz = pz (r' `atTime` time)
                   in time > 0 && rz >= 0 && rz <= h
