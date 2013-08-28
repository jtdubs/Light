{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Geometry.Quaternion
  -- ADT
  ( Quaternion, quaternion, qv, qw, toRotationMatrix, toAngleAxis

  -- Default Instances
  , identityQuaternion

  -- Arithmetic
  , normalizeQ, magnitudeQ, magnitudeSquaredQ, conjugate, (@*@), (@+@), (@-@), (@.@), (@*^)

  -- Transformations
  , rotationQuaternion, rotationQuaternion3
  )
where

import Control.Lens
import Control.Lens.TH
import Data.List

import Light.Geometry.Point
import Light.Geometry.Matrix
import Light.Geometry.Vector
import Light.Geometry.Normal

data Quaternion = Quaternion { _qv :: Vector, _qw :: Float }

makeLenses ''Quaternion

normalizeQ q@(Quaternion v w) = Quaternion (v^/s) (w/s)
  where s = magnitudeSquaredQ q

quaternion x y z w = normalizeQ $ Quaternion (vector x y z) w

instance Eq Quaternion where
  u == v = (u^.qv == v^.qv) && abs (u^.qw - v^.qw) < 0.0001

instance Show Quaternion where
  show (Quaternion v w) = concat ["#Q(", show v, ", ", show w, ")"]

identityQuaternion = quaternion 0 0 0 1

toRotationMatrix (Quaternion v w) =
  matrix [ 1 - 2 * (yy + zz),     2 * (xy - wz),     2 * (xz + wy), 0
         , 2 * (xy + wz)    , 1 - 2 * (xx + zz),     2 * (yz - wx), 0
         , 2 * (xz - wy)    ,     2 * (yz + wx), 1 - 2 * (xx + yy), 0
         , 0                ,                 0,                 0, 1 ]
  where x = v^.dx; y = v^.dy; z = v^.dz
        xx = x * x; yy = y * y; zz = z * z
        xy = x * y; xz = x * z; yz = y * z
        wx = w * x; wy = w * y; wz = w * z

toAngleAxis q@(Quaternion v w) = (acos w * 2, normalizeV v)

magnitudeQ = sqrt . magnitudeSquaredQ
magnitudeSquaredQ (Quaternion v w) = (v ^.^ v) + (w*w)

conjugate (Quaternion v w) = Quaternion (negateVector v) w

(Quaternion qv qw) @*@ (Quaternion rv rw) =
  Quaternion (cross qv rv ^+^ (qw *^ rv) ^+^ (rw *^ qv))
             (qw*rw - (qv ^.^ rv))

(Quaternion qv qw) @+@ (Quaternion rv rw) = Quaternion (qv ^+^ rv) (qw+rw)
(Quaternion qv qw) @-@ (Quaternion rv rw) = Quaternion (qv ^-^ rv) (qw+rw)
(Quaternion qv qw) @.@ (Quaternion rv rw) = (qv ^.^ rv) + (qw*rw)

q @*^ v
  | v == zeroVector = zeroVector
  | otherwise       = (q @*@ Quaternion v 0 @*@ conjugate q)^.qv

rotationQuaternion angle axis = Quaternion (normalizeV axis ^* sin (angle/2)) (cos (angle/2))

rotationQuaternion3 pitch yaw roll = quaternion (cr*sp*cy + sr*cp*sy)
                                                (cr*cp*sy - sr*sp*cy)
                                                (sr*cp*cy - cr*sp*sy)
                                                (cr*cp*cy + sr*sp*sy)
  where p = pitch/2; y = yaw/2; r = roll/2
        sp = sin p; sy = sin y; sr = sin r
        cp = cos p; cy = cos y; cr = cos r
