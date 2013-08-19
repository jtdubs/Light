{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Light.Data.Quaternion
	-- ADT
	( Quaternion

	-- Construction
	, quaternion, toList, fromList, toMatrix, toAngleAxis

	-- Default Instances
    , identity

	-- Arithmetic
	, magnitude, magnitudeSq, conjugate, (@*@), (@*^), (@*.)

	-- Transformations
	, rotate, rotate3
	)
where

import Light.Data.Point   (Point(..), origin, (.-.), (.+^))
import Light.Data.Matrix  (Matrix(..), matrix)
import Light.Data.Vector  (Vector(..), vector, normalized, zeroV, (^*))
import Data.Lens.Common   ((^.))
import Data.Lens.Template (makeLens)
import Data.List          (intersperse)

import qualified Light.Data.Vector as V

data Quaternion = Quaternion { _x :: !Float, _y :: !Float, _z :: !Float, _w :: !Float }

$(makeLens ''Quaternion)

quaternion :: Float -> Float -> Float -> Float -> Quaternion
quaternion x y z w = normalize $ Quaternion x y z w

toList :: Quaternion -> [Float]
toList (Quaternion x y z w) = [x, y, z, w]

fromList :: [Float] -> Quaternion
fromList [x, y, z, w] = Quaternion x y z w

toMatrix :: Quaternion -> Matrix
toMatrix (Quaternion x y z w) = matrix [ 1 - 2 * (yy + zz),     2 * (xy - wz),     2 * (xz + wy), 0,
        		                             2 * (xy + wz), 1 - 2 * (xx + zz),     2 * (yz - wx), 0,
        		                             2 * (xz - wy),     2 * (yz + wx), 1 - 2 * (xx + yy), 0,
        		                                         0,                 0,                 0, 1 ]
        	where xx = x * x; yy = y * y; zz = z * z
        	      xy = x * y; xz = x * z; yz = y * z
        	      wx = w * x; wy = w * y; wz = w * z

toAngleAxis :: Quaternion -> (Float, Vector)
toAngleAxis (Quaternion x y z w) = (acos(w) * 2, normalized $ vector x y z)

normalize :: Quaternion -> Quaternion
normalize q = fromList $ map (/ (magnitudeSq q)) (toList q)

identity :: Quaternion
identity = quaternion 0 0 0 1

magnitude :: Quaternion -> Float
magnitude = sqrt . magnitudeSq

magnitudeSq :: Quaternion -> Float
magnitudeSq = sum . map (\x -> x*x) . toList

conjugate :: Quaternion -> Quaternion
conjugate (Quaternion x y z w) = fromList [-x, -y, -z, w]

(@*@) :: Quaternion -> Quaternion -> Quaternion
(Quaternion qx qy qz qw) @*@ (Quaternion rx ry rz rw) =
  fromList [ qw*rx + qx*rw + qy*rz - qz*ry
           , qw*ry + qy*rw + qz*rx - qx*rz
           , qw*rz + qz*rw + qx*ry - qy*rx
           , qw*rw - qx*rx - qy*ry - qz*rz ]

(@*^) :: Quaternion -> Vector -> Vector
q @*^ v
  | v == zeroV = v
  | otherwise  = (^* (V.magnitude v))
               $ V.fromList
               $ take 3
               $ toList ( q
                      @*@ (fromList $ (++ [0]) $ V.toList $ V.normalized v)
                      @*@ (conjugate q))

(@*.) :: Quaternion -> Point -> Point
q @*. p = origin .+^ (q @*^ (p .-. origin))

rotate :: Float -> Vector -> Quaternion
rotate angle axis = quaternion ((n^.V.x)*s) ((n^.V.y)*s) ((n^.V.z)*s) c
  where s = sin (angle/2); c = cos (angle/2); n = V.normalized axis

rotate3 :: Float -> Float -> Float -> Quaternion
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
