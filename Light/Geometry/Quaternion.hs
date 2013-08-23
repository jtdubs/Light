{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Math.Quaternion
	-- ADT
	( Quaternion, quaternion, qx, qy, qz, qw, qs, toMatrix, toAngleAxis

	-- Default Instances
    , identityQuaternion

	-- Arithmetic
	, magnitude, magnitudeSquared, conjugate, (@*@), (@*^), (@*.), (@*!)

	-- Transformations
	, rotate, rotate3
	)
where

import Light.Geometry.Point  (originPoint, (.-.), (.+^))
import Light.Geometry.Matrix (matrix)
import Light.Geometry.Vector (vector, dx, dy, dz, zeroVector, (^*))
import Light.Geometry.Normal (normal, nx, ny, nz, (!*))
import Control.Lens          (Lens', (^.), lens, traversed, (//~))
import Control.Lens.TH       (makeLenses)
import Data.List             (intersperse)

import qualified Light.Geometry.Vector as V
import qualified Light.Geometry.Normal as N

data Quaternion = Quaternion { _qx :: Float, _qy :: Float, _qz :: Float, _qw :: Float }

makeLenses ''Quaternion

qs :: Lens' Quaternion [Float]
qs = lens (\ (Quaternion x y z w) -> [x, y, z, w]) (\q [x, y, z, w] -> Quaternion x y z w)

normalize q = (qs.traversed //~ (magnitudeSquared q)) q

quaternion x y z w = normalize $ Quaternion x y z w

instance Eq Quaternion where
  u == v = all (< 0.0001) $ map abs $ zipWith (-) (u^.qs) (v^.qs)

instance Show Quaternion where
  show q = "#Q(" ++ (concat . intersperse ", " . map show) (q^.qs) ++ ")"

identityQuaternion = quaternion 0 0 0 1

toMatrix (Quaternion x y z w) = matrix [ 1 - 2 * (yy + zz),     2 * (xy - wz),     2 * (xz + wy), 0,
        		                             2 * (xy + wz), 1 - 2 * (xx + zz),     2 * (yz - wx), 0,
        		                             2 * (xz - wy),     2 * (yz + wx), 1 - 2 * (xx + yy), 0,
        		                                         0,                 0,                 0, 1 ]
        	where xx = x * x; yy = y * y; zz = z * z
        	      xy = x * y; xz = x * z; yz = y * z
        	      wx = w * x; wy = w * y; wz = w * z

toAngleAxis (Quaternion x y z w) = (acos(w) * 2, V.normalize $ vector x y z)

magnitude = sqrt . magnitudeSquared
magnitudeSquared q = sum $ map (\x -> x*x) (q^.qs)

conjugate (Quaternion x y z w) = Quaternion (-x) (-y) (-z) w

(Quaternion qx qy qz qw) @*@ (Quaternion rx ry rz rw) =
  Quaternion (qw*rx + qx*rw + qy*rz - qz*ry)
             (qw*ry + qy*rw + qz*rx - qx*rz)
             (qw*rz + qz*rw + qx*ry - qy*rx)
             (qw*rw - qx*rx - qy*ry - qz*rz)

toQuaternionV v = Quaternion (v^.dx) (v^.dy) (v^.dz) 1
toQuaternionN n = Quaternion (n^.nx) (n^.ny) (n^.nz) 1

toVector q = vector (q^.qx) (q^.qy) (q^.qz)
toNormal q = normal (q^.qx) (q^.qy) (q^.qz)

q @*^ v
  | v == zeroVector = zeroVector
  | otherwise       = toVector (q @*@ (toQuaternionV $ V.normalize v) @*@ (conjugate q)) ^* (V.magnitude v)

q @*! n = toNormal (q @*@ (toQuaternionN $ N.normalize n) @*@ (conjugate q)) !* (N.magnitude n)

q @*. p = originPoint .+^ (q @*^ (p .-. originPoint))

rotate angle axis = quaternion ((n^.dx)*s) ((n^.dy)*s) ((n^.dz)*s) c
  where s = sin (angle/2); c = cos (angle/2); n = V.normalize axis

rotate3 pitch yaw roll = quaternion (sr*cp*cy - cr*sp*sy)
                                    (cr*sp*cy + sr*cp*sy)
                                    (cr*cp*sy - sr*sp*cy)
                                    (cr*cp*cy + sr*sp*sy)
  where p = pitch/2; y = yaw/2; r = roll/2
        sp = sin p; sy = sin y; sr = sin r;
        cp = cos p; cy = cos y; cr = cos r
