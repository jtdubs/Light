{-# LANGUAGE TemplateHaskell #-}

module Light.Data.Vector
    -- ADT
    ( Vector, vector, dx, dy, dz, ds, toList, fromList

    -- Default Instances
    , zeroVector, unitXVector, unitYVector, unitZVector

    -- Arithmetic
    , (^+^), (^-^), (^.^), (*^), (^*), (^/)
    , negateV, magnitude, magnitudeSq, normalize, cross
    )
where

import Control.Lens    (Lens', traverse, (*~), (//~), (^.), lens, sumOf)
import Control.Lens.TH (makeLenses)

data Vector = Vector { _dx :: Float, _dy :: Float, _dz :: Float }

vector = Vector

makeLenses ''Vector

toList (Vector x y z) = [x, y, z, 0]

fromList [x, y, z] = Vector x y z
fromList [x, y, z, 0] = Vector x y z

ds :: Lens' Vector [Float]
ds = lens toList (\v l -> fromList l)

instance Eq Vector where
  u == v = magnitudeSq (u ^-^ v) < 0.00001

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

u ^.^ v = sum $ toList $ vv (*) u v

negateV v = v ^* (-1)

magnitude = sqrt . magnitudeSq
magnitudeSq v = v ^.^ v

normalize v
 | magnitudeSq v == 0 = v
 | otherwise          = v ^/ magnitude v

cross u v = vector (u^.dy * v^.dz - u^.dz * v^.dy)
                   (u^.dz * v^.dx - u^.dx * v^.dz)
                   (u^.dx * v^.dy - u^.dy * v^.dx)
