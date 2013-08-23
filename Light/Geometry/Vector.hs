{-# LANGUAGE TemplateHaskell #-}

module Light.Geometry.Vector
    -- ADT
    ( Vector, vector, dx, dy, dz, ds

    -- Default Instances
    , zeroVector, unitXVector, unitYVector, unitZVector

    -- Arithmetic
    , (^+^), (^-^), (^.^), (*^), (^*), (^/)
    , negateVector, magnitude, magnitudeSquared, normalize, cross
    , angleBetween
    )
where

import Control.Lens    (Lens', traverse, (*~), (//~), (^.), lens)
import Control.Lens.TH (makeLenses)

data Vector = Vector { _dx :: Float, _dy :: Float, _dz :: Float }

vector = Vector

makeLenses ''Vector

ds :: Lens' Vector [Float]
ds = lens (\ (Vector x y z) -> [x, y, z, 0]) (\v [x, y, z, 0] -> Vector x y z)

instance Eq Vector where
  u == v = magnitudeSquared (u ^-^ v) < 0.00001

instance Show Vector where
  show (Vector x y z) = concat ["#V(", show x, ", ", show y, ", ", show z, ")"]

zeroVector  = vector 0 0 0
unitXVector = vector 1 0 0
unitYVector = vector 0 1 0
unitZVector = vector 0 0 1

vv op (Vector x y z) (Vector a b c) = Vector (op x a) (op y b) (op z c)

(^+^) = vv (+)
(^-^) = vv (-)
v ^* s = ds.traverse *~  s $ v
v ^/ s = ds.traverse //~ s $ v
(*^) = flip (^*)

u ^.^ v = sum $ (vv (*) u v)^.ds

negateVector v = v ^* (-1)

magnitude = sqrt . magnitudeSquared
magnitudeSquared v = v ^.^ v

normalize v
 | magnitudeSquared v == 0 = v
 | otherwise               = v ^/ magnitude v

cross u v = vector (u^.dy * v^.dz - u^.dz * v^.dy)
                   (u^.dz * v^.dx - u^.dx * v^.dz)
                   (u^.dx * v^.dy - u^.dy * v^.dx)

angleBetween v w = acos $ (v ^.^ w) / ((magnitude v) * (magnitude w))
