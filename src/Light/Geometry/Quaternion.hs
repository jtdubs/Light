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

import Light.Geometry.Matrix
import Light.Geometry.Vector

data Quaternion = Quaternion { _qv :: Vector, _qw :: Float }

makeLenses ''Quaternion

normalizeQ :: Quaternion -> Quaternion
normalizeQ q@(Quaternion v w) = Quaternion (v^/s) (w/s)
  where s = magnitudeSquaredQ q

quaternion :: Float -> Float -> Float -> Float -> Quaternion
quaternion x y z w = normalizeQ $ Quaternion (vector x y z) w

instance Eq Quaternion where
  u == v = (u^.qv == v^.qv) && abs (u^.qw - v^.qw) < 0.0001

instance Show Quaternion where
  show (Quaternion v w) = concat ["#Q(", show v, ", ", show w, ")"]

identityQuaternion :: Quaternion
identityQuaternion = quaternion 0 0 0 1

toRotationMatrix :: Quaternion -> Matrix
toRotationMatrix (Quaternion v w) =
  matrix [ 1 - 2 * (yy + zz),     2 * (xy - wz),     2 * (xz + wy), 0
         , 2 * (xy + wz)    , 1 - 2 * (xx + zz),     2 * (yz - wx), 0
         , 2 * (xz - wy)    ,     2 * (yz + wx), 1 - 2 * (xx + yy), 0
         , 0                ,                 0,                 0, 1 ]
  where x = v^.dx; y = v^.dy; z = v^.dz
        xx = x * x; yy = y * y; zz = z * z
        xy = x * y; xz = x * z; yz = y * z
        wx = w * x; wy = w * y; wz = w * z

toAngleAxis :: Quaternion -> (Float, Vector)
toAngleAxis (Quaternion v w) = (acos w * 2, normalizeV v)

magnitudeQ, magnitudeSquaredQ :: Quaternion -> Float
magnitudeQ = sqrt . magnitudeSquaredQ
magnitudeSquaredQ (Quaternion v w) = (v ^.^ v) + (w*w)

conjugate :: Quaternion -> Quaternion
conjugate (Quaternion v w) = Quaternion (negateV v) w

(@*@) :: Quaternion -> Quaternion -> Quaternion
(Quaternion v w) @*@ (Quaternion x y) =
  Quaternion (cross v x ^+^ (w *^ x) ^+^ (y *^ v))
             (w*y - (v ^.^ x))

(@+@), (@-@) :: Quaternion -> Quaternion -> Quaternion
(Quaternion v w) @+@ (Quaternion x y) = Quaternion (v ^+^ x) (w+y)
(Quaternion v w) @-@ (Quaternion x y) = Quaternion (v ^-^ x) (w+y)

(@.@) :: Quaternion -> Quaternion -> Float
(Quaternion v w) @.@ (Quaternion x y) = (v ^.^ x) + (w*y)

infixl 6 @+@, @-@
infixl 7 @*@, @.@

(@*^) :: Quaternion -> Vector -> Vector
q @*^ v
  | v == zeroVector = zeroVector
  | otherwise       = (q @*@ Quaternion v 0 @*@ conjugate q)^.qv

infixl 7 @*^

rotationQuaternion :: Float -> Vector -> Quaternion
rotationQuaternion angle axis = Quaternion (normalizeV axis ^* sin (angle/2)) (cos (angle/2))

rotationQuaternion3 :: Float -> Float -> Float -> Quaternion
rotationQuaternion3 pitch yaw roll = quaternion (cr*sp*cy + sr*cp*sy)
                                                (cr*cp*sy - sr*sp*cy)
                                                (sr*cp*cy - cr*sp*sy)
                                                (cr*cp*cy + sr*sp*sy)
  where p = pitch/2; y = yaw/2; r = roll/2
        sp = sin p; sy = sin y; sr = sin r
        cp = cos p; cy = cos y; cr = cos r
