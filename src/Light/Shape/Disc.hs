{-# LANGUAGE TemplateHaskell #-}

module Light.Shape.Disc
  -- ADT
  ( Disc, disc, discRadius

  -- Default Instances
  , unitDisc
  )
where

import Control.Monad
import Control.Lens hiding (transform)

import Light.Geometry.AABB
import Light.Geometry.Point
import Light.Geometry.Ray
import Light.Geometry.Transform
import Light.Geometry.Vector
import Light.Shape.Shape

data Disc = Disc { _discTransform :: Transform, _discRadius :: Double } deriving (Show, Read)

disc :: Double -> Disc
disc = Disc identityTransform

makeLenses ''Disc

unitDisc :: Disc
unitDisc = disc 1

instance Transformable Disc where
  transform t' (Disc t r) = Disc (compose t' t) r

instance Shape Disc where
  shapeTransform = discTransform

  bound (Disc _ r) = fromPoints [ point (-r) (-r) 0, point r r 0 ]

  surfaceArea (Disc _ r) = 2 * pi * r * r

  intersect theRay (Disc t r) = do
    guard $ abs rdz > 0.0001
    let time = -roz / rdz
    guard $ time >= 0
    let d = distanceSquared (r' `atTime` time) originPoint
    guard $ d <= r
    return time
    where r'  = transform (inverse t) theRay
          rdz = r'^.rayDirection.dz
          roz = r'^.rayOrigin.pz
