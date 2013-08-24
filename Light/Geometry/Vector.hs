{-# LANGUAGE TemplateHaskell #-}

module Light.Geometry.Vector
    -- ADT
    ( Vector, vector, dx, dy, dz, ds

    -- Default Instances
    , zeroVector, unitXVector, unitYVector, unitZVector

    -- Arithmetic
    , (^+^), (^-^), (^.^), (*^), (^*), (^/)
    , negateVector, magnitudeV, magnitudeSquaredV, normalizeV, cross
    , angleBetween
    )
where

import Control.Lens
import Control.Lens.TH

data Vector = Vector { _dx :: Float, _dy :: Float, _dz :: Float }

vector = Vector

makeLenses ''Vector

ds :: Lens' Vector [Float]
ds = lens (\ (Vector x y z) -> [x, y, z, 0]) (\v [x, y, z, 0] -> Vector x y z)

instance Eq Vector where
  u == v = magnitudeSquaredV (u ^-^ v) < 0.00001

instance Show Vector where
  show (Vector x y z) = concat ["#V(", show x, ", ", show y, ", ", show z, ")"]

zeroVector  = Vector 0 0 0
unitXVector = Vector 1 0 0
unitYVector = Vector 0 1 0
unitZVector = Vector 0 0 1

(Vector x y z) ^+^ (Vector a b c) = Vector (x+a) (y+b) (z+c)
(Vector x y z) ^-^ (Vector a b c) = Vector (x-a) (y-b) (z-c)

(Vector x y z) ^* s = Vector (x*s) (y*s) (z*s)
(Vector x y z) ^/ s = Vector (x/s) (y/s) (z/s)
s *^ (Vector x y z) = Vector (x*s) (y*s) (z*s)

(Vector x y z) ^.^ (Vector a b c) = (x*a) + (y*b) + (z*c)

negateVector (Vector x y z) = Vector (-x) (-y) (-z)

magnitudeV = sqrt . magnitudeSquaredV
magnitudeSquaredV v = v ^.^ v

normalizeV v
 | magnitudeSquaredV v == 0 = v
 | otherwise                = v ^/ magnitudeV v

cross (Vector ux uy uz) (Vector vx vy vz) =
  Vector (uy*vz - uz*vy)
         (uz*vx - ux*vz)
         (ux*vy - uy*vx)

angleBetween v w = acos $ (v ^.^ w) / ((magnitudeV v) * (magnitudeV w))
