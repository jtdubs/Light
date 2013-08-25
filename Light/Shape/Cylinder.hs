{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Cylinder
    -- ADT
    ( Cylinder, cylinder, cylinderTransform, cylinderRadius, cylinderHeight

    -- Default Instances
    , unitCylinder
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

data Cylinder = Cylinder { _cylinderTransform :: Transform, _cylinderRadius :: Float, _cylinderHeight :: Float }

cylinder = Cylinder identityTransform

makeLenses ''Cylinder

unitCylinder = cylinder 1 1

instance Show Cylinder where
  show (Cylinder t r h) = concat ["#C(", show t, ", ", show r, ", ", show h, ")"]

instance Transformable Cylinder where
  transform t' (Cylinder t r h) = Cylinder (compose t' t) r h

instance Shape Cylinder where
  shapeTransform = cylinderTransform

  bound (Cylinder _ r h) = fromPoints [ point (-r) (-r) 0, point r r h ]

  surfaceArea (Cylinder _ r h) = 2 * pi * r * h

  intersect ray (Cylinder t r h) = do
    ts <- liftM (filter f) $ quadratic a b c
    guard  $ length ts > 0
    return $ head ts
    where r' = transform (inverse t) ray
          rdx = r'^.rayDirection.dx
          rdy = r'^.rayDirection.dy
          rox = r'^.rayOrigin.px
          roy = r'^.rayOrigin.py
          a  = (rdx*rdx) + (rdy*rdy)
          b  = 2 * (rdx*rox) + (rdy*roy)
          c  = (rox*rox) + (roy*roy) - r*r
          f t = let rz = (r' `atTime` t)^.pz
                in t > 0 && rz >= 0 && rz <= h
