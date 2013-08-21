{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Data.Quaternion
	-- ADT
	( Quaternion, quaternion, qx, qy, qz, qw, qs, toList, fromList, toMatrix, toAngleAxis

	-- Default Instances
    , identity

	-- Arithmetic
	, magnitude, magnitudeSq, conjugate, (@*@), (@*^), (@*.)

	-- Transformations
	, rotate, rotate3
	)
where

import Light.Data.Point   (Point(..), originPoint, (.-.), (.+^))
import Light.Data.Matrix  (Matrix(..), matrix)
import Light.Data.Vector  (Vector(..), vector, dx, dy, dz, zeroVector, (^*))
import Control.Lens       (Lens', traverse, (*~), (//~), (^.), lens, traversed)
import Control.Lens.TH    (makeLenses)
import Data.List          (intersperse)

import qualified Light.Data.Vector as V

data Quaternion = Quaternion { _qx :: Float, _qy :: Float, _qz :: Float, _qw :: Float }

makeLenses ''Quaternion

quaternion x y z w = normalize $ Quaternion x y z w

toList (Quaternion x y z w) = [x, y, z, w]

fromList [x, y, z, w] = Quaternion x y z w

qs :: Lens' Quaternion [Float]
qs = lens toList (\q l -> fromList l)

identity = quaternion 0 0 0 1

toMatrix (Quaternion x y z w) = matrix [ 1 - 2 * (yy + zz),     2 * (xy - wz),     2 * (xz + wy), 0,
        		                             2 * (xy + wz), 1 - 2 * (xx + zz),     2 * (yz - wx), 0,
        		                             2 * (xz - wy),     2 * (yz + wx), 1 - 2 * (xx + yy), 0,
        		                                         0,                 0,                 0, 1 ]
        	where xx = x * x; yy = y * y; zz = z * z
        	      xy = x * y; xz = x * z; yz = y * z
        	      wx = w * x; wy = w * y; wz = w * z

toAngleAxis (Quaternion x y z w) = (acos(w) * 2, V.normalize $ vector x y z)

normalize q = (qs.traversed //~ (magnitudeSq q)) q

magnitude = sqrt . magnitudeSq
magnitudeSq = sum . map (\x -> x*x) . toList

conjugate (Quaternion x y z w) = fromList [-x, -y, -z, w]

(Quaternion qx qy qz qw) @*@ (Quaternion rx ry rz rw) =
  fromList [ qw*rx + qx*rw + qy*rz - qz*ry
           , qw*ry + qy*rw + qz*rx - qx*rz
           , qw*rz + qz*rw + qx*ry - qy*rx
           , qw*rw - qx*rx - qy*ry - qz*rz ]

q @*^ v
  | v == zeroVector = v
  | otherwise  = (^* (V.magnitude v))
               $ V.fromList
               $ take 3
               $ toList ( q
                      @*@ (fromList $ V.toList $ V.normalize v)
                      @*@ (conjugate q))

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

instance Eq Quaternion where
  u == v = all (< 0.0001) $ map abs $ zipWith (-) (toList u) (toList v)

instance Show Quaternion where
  show q = "#Q(" ++ (concat . intersperse ", " . map show . toList) q ++ ")"
