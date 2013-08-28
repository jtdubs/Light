{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Disc
    -- ADT
    ( Disc, disc, discTransform, discRadius

    -- Default Instances
    , unitDisc
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

data Disc = Disc { _discTransform :: Transform, _discRadius :: Float }

disc = Disc identityTransform

makeLenses ''Disc

unitDisc = disc 1

instance Show Disc where
  show (Disc t r) = concat ["#D(", show t, ", ", show r, ")"]

instance Transformable Disc where
  transform t' (Disc t r) = Disc (compose t' t) r

instance Shape Disc where
  shapeTransform = discTransform

  bound (Disc _ r) = fromPoints [ point (-r) (-r) 0, point r r 0 ]

  surfaceArea (Disc _ r) = 2 * pi * r * r

  intersect ray (Disc t r) = do
    guard $ abs rdz > 0.0001
    let t = -roz / rdz
    guard $ t >= 0
    let d = distanceSquared (r' `atTime` t) originPoint
    guard $ d <= r
    return t
    where r' = transform (inverse t) ray
          rdz = r'^.rayDirection.dz
          roz = r'^.rayOrigin.pz
