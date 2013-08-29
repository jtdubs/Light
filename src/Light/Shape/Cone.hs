{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Cone
  -- ADT
  ( Cone, cone, coneTransform, coneRadius, coneHeight

  -- Default Instances
  , unitCone
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

data Cone = Cone { _coneTransform :: Transform, _coneRadius :: Float, _coneHeight :: Float }

cone :: Float -> Float -> Cone
cone = Cone identityTransform

makeLenses ''Cone

unitCone :: Cone
unitCone = cone 1 1

instance Show Cone where
  show (Cone t r h) = concat ["#S(", show t, ", ", show r, ", ", show h, ")"]

instance Transformable Cone where
  transform t' (Cone t r h) = Cone (compose t' t) r h

instance Shape Cone where
  shapeTransform = coneTransform

  bound (Cone _ r h) = fromPoints [ point (-r) (-r) 0, point r r h ]

  surfaceArea (Cone _ r h) = pi * r * sqrt (r*r + h*h)

  intersect ray (Cone t r h) = do
    ts <- liftM (filter f) $ quadratic a b c
    guard  $ not (null ts)
    return $ head ts
    where r' = transform (inverse t) ray
          rdx = r'^.rayDirection.dx
          rdy = r'^.rayDirection.dy
          rdz = r'^.rayDirection.dz
          rox = r'^.rayOrigin.px
          roy = r'^.rayOrigin.py
          roz = r'^.rayOrigin.pz
          a   = (  h*h*rdx*rdx +   h*h*rdy*rdy)/(r*r) + (  -rdz*rdz)
          b   = (2*h*h*rox*rdx + 2*h*h*roy*rdy)/(r*r) + (-2*roz*rdz + 2*rdz*h)
          c   = (  h*h*rox*rox +   h*h*roy*roy)/(r*r) + (  -roz*roz + 2*roz*h - h*h)
          f t = let rz = (r' `atTime` t)^.pz
                in t > 0 && rz >= 0 && rz <= h
